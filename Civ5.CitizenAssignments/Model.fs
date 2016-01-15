[<AutoOpen>]
module Model

open System

///Enumeration of possible yield types
type YieldType =
    | Food = 0
    | Gold = 1
    | Production = 2
    | Science = 3
    | Faith = 4
    | Culture = 5
    | GreatArtist = 6
    | GreatWriter = 7
    | GreatMusician = 8
    | GreatEngineer = 9
    | GreatScientist = 10
    | GreatMerchant = 11    

///Contains functions and types modeling a tile or building yield
[<RequireQualifiedAccess>]
module Yield =

    ///SCU
    type Instance = Yield of (Int32 * YieldType)

    ///Contains error messages used by the module
    [<RequireQualifiedAccess>]
    module private ErrorMessages = 
        let [<Literal>] InvalidValue = "The yield number cannot be less than 1."

    ///Contains helper functions used by the module internally
    [<AutoOpen>]
    module private Helpers = 

        ///Calls f when the given yield value is valid, otherwise throws an exception
        let whenValid f value = 
            if (value > 0) then
                f value
            else
                failwith ErrorMessages.InvalidValue

    ///Creates a new yield
    let create type' = whenValid (fun value -> Yield (value, type'))

    ///Gets the value of a yield
    let getValue (Yield (value, _)) = value

    ///Gets the type of a yield
    let getType (Yield (_, type')) = type'

    ///True if a yield is of a given type
    let isOf type' = getType >> ((=) type')

    ///Reduces a list of yields by summing them by type
    let reduce = 
        Seq.map (fun (Yield (value, type')) -> value, type')
        >> Seq.groupBy snd
        >> Seq.map (fun (type', values) -> 
                let value = Seq.sumBy fst values
                in Yield (value, type')
            )
        >> Seq.toList

    ///Applies a multiplier to a yield value
    let multiply factor (Yield (value, type')) = Yield (value * factor, type')

///Discriminated union of asset types
type AssetType = 
    | Tile 
    | Building of Int32

///Contains functions and types modeling a city asset
[<RequireQualifiedAccess>]
module Asset = 

    ///Record containing asset data
    type Data = {
        Id : String;
        Type : AssetType;
        Yields : Yield.Instance list;
    }

    ///SCU
    type Instance = Asset of Data

    ///Contains error messages used by the module
    [<RequireQualifiedAccess>]
    module private ErrorMessages = 
        let [<Literal>] InvalidCapacity = "A capacity must be at least one."

    ///Contains helper functions used by the module internally
    [<AutoOpen>]
    module private Helpers = 

        ///Apply f to the data of an asset
        let apply f (Asset data) = f data

    ///Creates a new building
    let createBuilding name capacity yields = 
        if (capacity < 1) then
            failwith ErrorMessages.InvalidCapacity
        else
            Asset ({ Id = name; Type = (Building capacity); Yields = yields; })

    ///Creates a new tile
    let createTile position yields = Asset ({ Id = position; Type = Tile; Yields = yields; })

    ///Gets the ID of an asset
    let getId = apply (fun data -> data.Id)

    ///Gets the citizen capacity of an asset
    let getCapacity = apply (fun data ->
        match data.Type with
        | Building capacity -> capacity
        | _ -> 1
    )

    ///True if an asset is a tile
    let isTile = apply (fun data -> data.Type = Tile)

    ///True if an asset is a building
    let isBuilding = isTile >> not

    ///Gets the yields associated with an asset
    let getYields = apply (fun data -> data.Yields)

    ///Gets an asset's yield value by type
    let getYieldOf type' =
        getYields
        >> List.tryFind (Yield.isOf type')
        >> Option.map Yield.getValue
        >> Option.unwrap 0

///Contains functions and types modeling a city
[<RequireQualifiedAccess>]
module City =     

    ///Record containing data about a city's citizens
    type CitizenData = {
        Unemployed : Int32;
        Employed : Int32;        
        BaseYields : Yield.Instance list;
    }

    ///Record containing data about a city
    type CityData = {
        BaseYields : Yield.Instance list; //NOTE Yields from the city tile and buildings
        Citizens : CitizenData;
        Assets : (Asset.Instance * Int32) list;
    }

    ///SCU
    type Instance = City of CityData

    ///Contains constants used by this module
    [<RequireQualifiedAccess>]
    module private Constants =
        let [<Literal>] MaxTiles = 36
        let [<Literal>] MinSize = 1

    ///Contains error messages used by this module
    [<RequireQualifiedAccess>]
    module private ErrorMessages = 
        let [<Literal>] InvalidYields = "At least one yield is required."
        let [<Literal>] InvalidSize = "The city must have at least one citizen."
        let InvalidTiles = sprintf "The city must have at most %u tiles." Constants.MaxTiles
        let [<Literal>] DuplicateAssetIds = "The city assets must have unique IDs."
        let AssetNotFound = sprintf "Could not find asset with ID %s."
        let AssetAssigned = sprintf "Asset with ID %s is already fully assigned."
        let AssetUnassigned = sprintf "Asset with ID %s is not currently assigned."
        let [<Literal>] NoUnemployedCitizens = "There are no unemployed citizens which can be assigned to an asset."

    ///Contains helper functions used internally by this module
    [<AutoOpen>]
    module private Helpers = 
       
        ///Accepts parameters for a new city and calls f if they are valid, throwing an exception otherwise
        let whenValid f =

            let getTileCount =
                List.filter Asset.isTile
                >> List.length

            let containsDuplicateIds =
                Seq.groupBy Asset.getId
                >> Seq.filter (snd >> Seq.length >> ((<>) 1))
                >> Seq.isEmpty
                >> not
        
            fun cityYields citizenYields assets size ->
                if (List.isEmpty cityYields || List.isEmpty citizenYields) then
                    failwith ErrorMessages.InvalidYields
                else if(size < Constants.MinSize) then
                    failwith ErrorMessages.InvalidSize
                else if ((getTileCount assets) > Constants.MaxTiles) then
                    failwith ErrorMessages.InvalidTiles
                else if(containsDuplicateIds assets) then
                    failwith ErrorMessages.DuplicateAssetIds
                else
                    f cityYields citizenYields assets size

        ///Apply f to the city's data
        let apply f (City data) = f data

        ///Contains functions for working with a city's list of assets
        [<RequireQualifiedAccess>]
        module Assets = 

            ///Detach an asset by it's ID - returning the asset with the given ID and the rest of the city's assets separately
            let detach assetId assets = 

                let partitioned = 
                    assets
                    |> List.partition (fun (asset, _) -> (Asset.getId asset) = assetId)
                
                match partitioned with
                | [ asset ], assets -> Some (asset, assets)
                | _ -> None

            ///Initialises a list of assets as unassigned
            let init assets = 
                List.map (fun asset -> (asset, 0)) assets

            ///Gets a list of assets by assignment status
            let getByStatus assigned = 

                let predicate citizens = 
                    if assigned then
                        citizens > 0
                    else
                        citizens = 0

                List.choose (fun (asset : Asset.Instance, citizens) -> 
                    if (predicate citizens) then
                        Some (asset, citizens)
                    else
                        None
                )

        ///Contains functions used to work with a city's citizens
        [<RequireQualifiedAccess>]
        module Citizens = 
            
            ///Create default citizen data for a city of a given size
            let defaultOf size baseYields = 
                {
                    Unemployed = size; 
                    Employed = 0;
                    BaseYields = baseYields;
                }

            ///Assign a citizen
            let assign citizens = 
                { citizens with
                    Unemployed = (citizens.Unemployed - 1);
                    Employed = (citizens.Employed + 1);
                }

            ///Unassign a citizen
            let unassign citizens = 
                { citizens with
                    Unemployed = (citizens.Unemployed + 1);
                    Employed = (citizens.Employed - 1);
                }

            ///Get a list of assignments for a city's citizens
            let getAssignments assets citizens = 

                let employed = 
                    assets        
                    |> Assets.getByStatus true 
                    |> List.map (fun (asset, citizens) -> ((Some asset), citizens))

                let unemployed = (None, citizens.Unemployed)
    
                unemployed :: employed

    ///Creates a new city
    let create = 
        whenValid (fun cityYields citizenYields assets size ->
            let data =
                {
                    BaseYields = cityYields;
                    Citizens = (Citizens.defaultOf size citizenYields);
                    Assets = (Assets.init assets);
                }
            in (City data)
        )

    ///Get the total population size of a city
    let getSize = apply (fun data -> data.Citizens.Employed + data.Citizens.Unemployed)

    ///Get the food required to feed a city
    let getRequiredFood = getSize >> ((*) 2) //TODO Support for policies etc. which change food requirements

    ///Get a list of the assigned assets for a city
    let getAssignedAssets = apply (fun data -> Assets.getByStatus true data.Assets)

    ///Get a list of unassigned assets for a city
    let getUnassignedAssets = apply (fun data -> Assets.getByStatus false data.Assets)

    ///Get a list of the assets in a city to which a citizen can still be assigned
    let getAssignableAssets = apply (fun data -> 
        data.Assets
        |> List.filter (fun (asset, citizens) ->
                (Asset.getCapacity asset) > citizens
            )
        |> List.map fst
    )

    ///True if a city has unemployed citizens
    let hasUnemployedCitizens = apply (fun data -> data.Citizens.Unemployed > 0)

    ///Gets the number of unemployed citizens in a city
    let getUnemployedCitizens = apply (fun data -> data.Citizens.Unemployed)

    ///Gets list of citizens along with the asset to which they are assigned
    let getCitizenAssignments = apply (fun data -> Citizens.getAssignments data.Assets data.Citizens)

    ///Get a list of the total yields for a city
    let getYields = apply (fun data ->
        Citizens.getAssignments data.Assets data.Citizens
        |> List.map (function 
                | (Some asset, citizens) -> (Asset.getYields asset), citizens
                | (_, citizens) -> data.Citizens.BaseYields, citizens
            )
        |> List.collect (fun (yields, citizens) -> List.map (Yield.multiply citizens) yields)
        |> List.append data.BaseYields
        |> Yield.reduce
    )

    //Gets the base yield of a given type for a city
    let getBaseYieldOf type' = apply (fun data -> 
        data.BaseYields
        |> List.tryFind (Yield.isOf type')
        |> Option.map Yield.getValue
        |> Option.unwrap 0
    )

    ///Get the total yield of a given type for a city
    let getYieldOf type' = 
        getYields
        >> List.tryFind (Yield.isOf type')
        >> Option.map Yield.getValue
        >> Option.unwrap 0

    ///Get the maximum possible yield for a given type for a city
    let getMaximumYieldOf type' = apply (fun data ->

        let citizens = (data.Citizens.Employed + data.Citizens.Unemployed)

        //Convert assets into a list of yields of the given type
        //and then just take as many as can be assigned to given the total
        //number of citizens in the city and sum. Then add the base yield for that
        //type.
        let yieldAssets = 
            data.Assets
            |> List.map (fun (asset, _) -> 
                    Asset.getCapacity asset, 
                    Asset.getYieldOf type' asset
                )
            |> List.filter (fun (_, yield') -> yield' > 0)
            |> List.collect (fun (capacity, yield') ->
                    List.init capacity (fun _ -> yield')
                )
            |> List.sort
            |> List.rev

        let assetCount = List.length yieldAssets

        let assignCount, unassignCount = 
            if (citizens <= assetCount) then
                citizens, 0
            else
                assetCount, (citizens - assetCount)            

        let assignedYield =
            yieldAssets
            |> Seq.take assignCount
            |> Seq.sum

        let unassignedYield = 
            if (unassignCount > 0) then
                data.Citizens.BaseYields
                |> List.tryFind (Yield.isOf type')
                |> Option.map (Yield.multiply unassignCount)
                |> Option.map Yield.getValue
                |> Option.unwrap 0
            else
                0

        let baseYield = 
            data.BaseYields
            |> List.tryFind (Yield.isOf type')
            |> Option.map Yield.getValue
            |> Option.unwrap 0

        (baseYield + assignedYield + unassignedYield)
    )

    ///True if an asset can have a citizen assigned to it
    let isAssignable assetId = apply (fun data ->
        match (Assets.detach assetId data.Assets) with
        | Some ((asset, citizens), _) when ((Asset.getCapacity asset) > citizens) -> true
        | _ -> false
    )

    ///Assigns an unemployed citizen to a given asset
    let assign assetId = apply (fun data ->
        if (data.Citizens.Unemployed > 0) then
            match (Assets.detach assetId data.Assets) with
            | Some ((asset, citizens), others) when ((Asset.getCapacity asset) > citizens) -> 
                let data' =
                    { data with 
                        Assets = ((asset, (citizens + 1)) :: others); 
                        Citizens = (Citizens.assign data.Citizens);
                    }
                in (City data')

            | Some _ -> invalidOp (ErrorMessages.AssetAssigned assetId)
            | _ -> failwith (ErrorMessages.AssetNotFound assetId)
        else
            invalidOp ErrorMessages.NoUnemployedCitizens
    )

    ///Unassigns a citizen from a given asset
    let unassign assetId = apply (fun data ->
        match (Assets.detach assetId data.Assets) with
        | (Some ((asset, citizens), others)) when (citizens > 0) -> 
            let data' = 
                { data with
                    Assets = ((asset, (citizens - 1)) :: others);
                    Citizens = (Citizens.unassign data.Citizens);
                }
            in (City data')
        | (Some _) -> invalidOp (ErrorMessages.AssetUnassigned assetId)
        | _ -> failwith (ErrorMessages.AssetNotFound assetId)
    )

    ///Unassigns all citizens
    let unassignAll = apply (fun data ->

        let assets = 
            data.Assets 
            |> List.map (fun (asset, _) -> (asset, 0))

        let citizens = 
            { data.Citizens with
                Unemployed = (data.Citizens.Employed + data.Citizens.Unemployed);
                Employed = 0;
            }

        City ({ data with Assets = assets; Citizens = citizens; })
    )