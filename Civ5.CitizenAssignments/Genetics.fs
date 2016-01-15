//Contains functions for evolving populations
[<AutoOpen>]
module Genetics

open System

///Types used to describe functions used to evolve a population

type CombineFunction<'T> = 'T -> 'T -> 'T
type EvaluateFunction<'T> = 'T -> Decimal
type MutateFunction<'T> = 'T -> 'T

///Record containing the settings for the main algorithm
type AlgorithmSettings<'T> = {
    IsElitist : Boolean; 
    TournamentSize : Int32;
    MutationRate : Decimal;
    Combine : CombineFunction<'T>;
    Mutate : MutateFunction<'T>;
    Evaluate : EvaluateFunction<'T>;
    Population : 'T list;
}

///Contains functions for running tournament selection
[<RequireQualifiedAccess>]
module Tournament = 

    ///Run a tournament using the given settings
    let run settings = 
        settings.Population
        |> List.selectMany settings.TournamentSize
        |> List.maxBy settings.Evaluate

///Contains functions for manipulating populations
[<RequireQualifiedAccess>]
module Population = 

    ///Breeds the members of a population selected via tournament to create a new population of the same size
    let combine settings = 

        let init _ = 

            //TODO This allows "self breeding". Hmm.

            let x = Tournament.run settings
            let y = Tournament.run settings

            settings.Combine x y

        let size = List.length settings.Population

        let offspring = Array.Parallel.init size init
        in Array.toList offspring

    ///Mutates the members of a population using the settings given
    let mutate settings individual = 
        if (RNG.rate settings.MutationRate) then
            settings.Mutate individual
        else
            individual

    ///Evolves a population by breeding and mutation to create a new population of the same size
    let evolve settings = 

        let offspring = 
            if settings.IsElitist then

                let fittest, remaining = 
                    settings.Population
                    |> List.sortBy settings.Evaluate
                    |> List.rev
                    |> List.detach

                let settings' = { settings with Population = remaining; }
                in fittest :: (combine settings')

            else
                combine settings
        
        offspring
        |> Array.ofList
        |> Array.Parallel.map (mutate settings)
        |> Array.toList
    
///Contains functions for running genetic algorithms
[<RequireQualifiedAccess>]
module Algorithm = 

    ///Evolves a population for a given number of generations, returning
    ///the evolved population and a history of the fittest members of each generation
    let run generations settings = 

        let rec runWithHistory generations (history, settings) = 
            if (generations = 0) then

                let history' = 
                    history
                    |> List.rev
                    |> List.index
                in (history', settings.Population)

            else
                let population = Population.evolve settings            
                let fittest = List.maxBy settings.Evaluate population

                let generations' = (generations - 1)
                let history' = fittest :: history
                let settings' = { settings with Population = population; }

                runWithHistory generations' (history', settings')
    
        runWithHistory generations ([], settings)

