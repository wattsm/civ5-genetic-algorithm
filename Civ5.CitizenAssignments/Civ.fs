///Contains functions for evolving a city via genetic algorithms
[<AutoOpen>]
module Civ

open System

///Record containing settings for the advisor module
type AdvisorSettings = {
    PopulationSize : Int32;
    TournamentSize : Int32;
    Generations : Int32;
    MutationRate : Decimal;
    Weightings : (YieldType * Decimal) list;
}
with
    static member Default =
        {
            PopulationSize = 40;
            TournamentSize = 8;
            Generations = 100;
            MutationRate = 0.3M;
            Weightings = 
                [
                    (YieldType.Food, 1M);
                    (YieldType.Production, 1M);
                    (YieldType.Gold, 1M);
                    (YieldType.Faith, 1M);
                    (YieldType.Culture, 1M);
                    (YieldType.Science, 1M);
                    (YieldType.GreatArtist, 1M);
                    (YieldType.GreatWriter, 1M);
                    (YieldType.GreatMusician, 1M);
                    (YieldType.GreatEngineer, 1M);
                    (YieldType.GreatScientist, 1M);
                    (YieldType.GreatMerchant, 1M);
                ];
        }

///Contains functions for optimising a city's citizen assignments
[<RequireQualifiedAccess>]
module Advisor =

    ///Contains helper functions used by this module
    [<AutoOpen>]
    module private Helpers = 

        let defaultWeighting = 0M //NOTE Cannot create a literal decimal

        ///Active pattern used to determine the growth state of a city
        let (|Growing|Stagnant|Starving|) city = 

            let required = City.getRequiredFood city
            let actual = City.getYieldOf YieldType.Food city

            if (required < actual) then
                Growing
            else if (required = actual) then    
                Stagnant
            else
                Starving            

        ///Randomly assigns a given number of citizens to assets or unemployment 
        let rec assignCitizensRandomly count city = 
            if (count = 0) then
                city
            else

                let city' = 
                    if (RNG.rate 0.25M) then
                        city
                    else

                        let assetId = 
                            city
                            |> City.getAssignableAssets
                            |> List.map Asset.getId
                            |> List.selectOne

                        City.assign assetId city

                assignCitizensRandomly (count - 1) city'

        ///Assigns citizens to assets until a city's food requirements are met
        let assignCitizensForFood  =

            //TODO What if there is not enough food available? Check against assignable assets and unemployed citizens

            let rec assignCitizens city required actual =
                if (actual >= required) then
                    city
                else
                    
                    let assetId, food = 
                        City.getAssignableAssets city
                        |> List.map (fun asset -> (Asset.getId asset, Asset.getYieldOf YieldType.Food asset))
                        |> List.maxBy snd

                    let city' = City.assign assetId city
                    let actual' = (actual + food)

                    assignCitizens city' required actual'

            fun city ->

                let required = City.getRequiredFood city
                let actual = City.getYieldOf YieldType.Food city

                assignCitizens city required actual

        ///Creates a new population of city citizen assignments based on a given city
        let createPopulation populationSize city = 

            let city' = City.unassignAll city

            List.init populationSize (fun _ -> 

                let viableCity = assignCitizensForFood city'
                let unemployed = City.getUnemployedCitizens viableCity
                
                if (unemployed > 0) then
                    assignCitizensRandomly unemployed viableCity
                else
                    viableCity
            )

        ///Evaulates the fitness of a city based on the given weightings
        let evaluate weightings =

            //TODO Penalty for stagnant cities?

            let getWeightedValue yield' = 

                let type' = Yield.getType yield'
                let value = decimal (Yield.getValue yield')

                let factor = 
                    weightings
                    |> List.tryFind (fst >> ((=) type'))
                    |> Option.map snd
                    |> Option.unwrap defaultWeighting

                (value * factor)  
        
            fun city ->
                match city with
                | Starving -> 0M
                | _ -> 
                    city
                    |> City.getYields
                    |>  List.sumBy getWeightedValue

        ///Combines two city citizen assignments
        let combine =
    
            //Get a flattened list of assignments - e.g. [ (asset, 2); ] becomes [ asset; asset; ]
            let getAssignments =
                City.getCitizenAssignments
                >> List.collect (fun (asset, count) -> List.init count (fun _ -> asset))

            //Random chose between a city X and a city Y assignment
            let pickAssignment (assignmentX, assignmentY) = 
                if (RNG.flip ()) then
                    assignmentX
                else
                    assignmentY

            //Apply an assingment if possible. If not, the citizen is unemployed.
            let applyAssignment city asset = 

                let assetId = Asset.getId asset

                if (City.isAssignable assetId city) then
                    City.assign assetId city
                else
                    city
            
            fun cityX cityY -> 
                let assignmentsX = getAssignments cityX
                let assignmentsY = getAssignments cityY

                let unassignedCity = City.unassignAll cityX

                List.zip assignmentsX assignmentsY
                |> List.map pickAssignment
                |> List.choose id               
                |> List.fold applyAssignment unassignedCity

        ///Mutate a city by randomly re- or un-assigning a citizen
        let mutate city = 

            let unassign, assign = 
                if ((RNG.flip ()) && (City.hasUnemployedCitizens city)) then
                    //50% chance of assigning an unemployed citizen, if possible...
                    false, true 
                else
                    //Otherwise definitely unassign a citizen and 50% chance of assigning them elsewhere
                    true, (RNG.flip ())

            let city' = 
                if unassign then
                    let assetId = 
                        city
                        |> City.getAssignedAssets
                        |> List.map (fst >> Asset.getId)
                        |> List.selectOne 
                    in City.unassign assetId city
                else
                    city

            if assign then

                //NOTE This can potentially assign a citizen back where they came from.

                let assets = City.getAssignableAssets city'

                match assets with
                | [] -> city'
                | _ -> 
                    let assetId = 
                        assets
                        |> List.map Asset.getId
                        |> List.selectOne
                    in City.assign assetId city'
                    
            else
                city'

    ///Optimises a city's citizen assignments according to the given settings
    let optimise advisorSettings city =

        let evaluate' = evaluate advisorSettings.Weightings
        let population = createPopulation advisorSettings.PopulationSize city

        let algorithmSettings = 
            {
                IsElitist = false;
                TournamentSize = advisorSettings.TournamentSize;
                MutationRate = advisorSettings.MutationRate;
                Combine = combine;
                Evaluate = evaluate';
                Mutate = mutate;
                Population = population;
            }

        let history, _ = Algorithm.run advisorSettings.Generations algorithmSettings

        let history' =
            history
            |> List.map (fun (index, city') ->
                    let fitness = evaluate' city'
                    in (index, city', fitness)
                )

        let (_, fittest, fitness) = 
            history'
            |> List.rev
            |> List.head
    
        (fittest, fitness, history')
    