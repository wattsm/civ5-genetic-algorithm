module Program 

open System

[<EntryPoint>]
let main _ = 

    let settings = 
        { AdvisorSettings.Default with
            Weightings = 
                [
                    (YieldType.Science, 1M);
                ];
        }

    //let original = Data.load "Constantinople.json"
    let original = Data.load "Mecca.json"
    let optimised, fitness, history = Advisor.optimise settings original

    let scores = 
        let values = List.map (fun (_, _, fitness) -> fitness) history
        in String.Join (", ", (List.toArray values))

    printfn "HISTORY"
    printfn "===================="
    printfn "%s" scores
    printfn ""

    printfn "CITY"
    printfn "===================="
    printfn "Fitness: %M" fitness
    printfn "Yields:"

    let yields = 
        City.getYields optimised
        |> Seq.sortBy (Yield.getType >> int)

    for yield' in yields do
        
        let type' = Yield.getType yield'
        let value = Yield.getValue yield'
        let max = City.getMaximumYieldOf type' optimised
        let base' = City.getBaseYieldOf type' optimised

        printfn "  %O: %i / %i (base: %i)" type' value max base'

    printfn ""

    printfn "CITIZENS"
    printfn "===================="

    //Sort with tiles first, then buildings, then unassigned citizens
    let assignments = 
        City.getCitizenAssignments optimised
        |> List.sortBy (fun (asset, _) ->
                match asset with
                | Some asset' when Asset.isTile asset' -> (1, Asset.getId asset') //Tiles by position
                | Some asset' -> (2, Asset.getId asset') //Buildings by name
                | _ -> (3, "X") //Unemployed
            )

    for (asset, citizens) in assignments do
        match asset with
        | Some x when Asset.isTile x -> printfn  "Tile: %s" (Asset.getId x)
        | Some x -> printfn  "Building: %s (%i/%i)" (Asset.getId x) citizens (Asset.getCapacity x)
        | _ -> printfn "Unemployed: %u" citizens    

    Console.ReadLine ()
    |> ignore

    0 // return an integer exit code
