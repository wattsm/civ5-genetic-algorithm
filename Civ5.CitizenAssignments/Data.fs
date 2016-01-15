///Contains functions for loading example data from the embedded Data.json file
[<RequireQualifiedAccess>]
module Data

open System
open System.IO
open System.Reflection
open Newtonsoft.Json.Linq

[<AutoOpen>]
module private Helpers = 

    let yieldMappings = 
        dict [
            ("GOLD", YieldType.Gold);
            ("PRODUCTION", YieldType.Production);
            ("SCIENCE", YieldType.Science);
            ("FAITH", YieldType.Faith);
            ("FOOD", YieldType.Food);
            ("CULTURE", YieldType.Culture);
            ("SCIENTIST", YieldType.GreatScientist);
            ("ENGINEER", YieldType.GreatEngineer);
            ("MERCHANT", YieldType.GreatMerchant);
            ("WRITER", YieldType.GreatWriter);
            ("MUSICIAN", YieldType.GreatMusician);
            ("ARTIST", YieldType.GreatArtist);
        ]        

    module Json = 

        let getValue (path : String) (data : JObject) = 
            (data.SelectToken path) :?> JValue

        let getChild (path : String) (data : JObject) = 
            (data.SelectToken path) :?> JObject

        let mapObjects (path : String) f (data : JObject) = 
            data.SelectTokens path
            |> Seq.cast<JObject>
            |> Seq.map f
            |> Seq.toList

        let mapArray (path :  String) f (data : JObject) = 

            let array = (data.SelectToken path) :?> JArray

            array.Values<JObject> ()
            |> Seq.map f
            |> Seq.toList

    let readYields (data : JObject) = 
        data.Properties ()
        |> Seq.choose (fun property -> 
                match property.Value with
                | :? JValue as value when (value.Value <> null) ->
                    match value.Type with
                    | JTokenType.Integer -> 

                        let name = property.Name.ToUpper ()
                        let value' = Convert.ToInt32 (value.Value)

                        Some (name, value')
                        
                    | _ -> None
                | _ -> None
            )                
        |> Seq.map (fun (name, value) ->
                let type' = yieldMappings.[name]
                in Yield.create type' value
            )
        |> Seq.toList

    let readTile (data : JObject) = 

        let position = 
            let value = Json.getValue "$.pos" data
            in (string value.Value)

        let yields =  readYields data

        (position, yields)

    let readBuilding (data : JObject) = 
        
        let name = string (Json.getValue "$.name" data)
        let capacity = Convert.ToInt32 (Json.getValue "$.capacity" data)
        let yields = readYields (Json.getChild "$.yields.citizen" data)

        Asset.createBuilding name capacity yields

    let readBaseYields (data : JObject) = 

        let tile = readYields (Json.getChild "$.yields.tile" data)
        let buildings = Json.mapObjects "$.buildings..yields.base" readYields data

        (tile :: buildings)
        |> List.collect id
        |> Yield.reduce        

    let readCity (data : JObject) = 

        let size = Convert.ToInt32 (Json.getValue "$.size" data)
        let baseYields = readBaseYields data
        let citizenYields = readYields (Json.getChild "$.yields.citizen" data)

        let tiles = 
            data
            |> Json.mapArray "$.tiles" readTile 
            |> List.map (fun (position, yields) -> Asset.createTile position yields)

        let buildings = Json.mapObjects "$.buildings[?(@.capacity > 0)]" readBuilding data

        City.create baseYields citizenYields (tiles @ buildings) size

let load filename = 
    
    let assembly = Assembly.GetExecutingAssembly ()
    use stream = assembly.GetManifestResourceStream filename
    use reader = new StreamReader (stream)

    let json = reader.ReadToEnd ()
    let jobject = JObject.Parse json

    readCity jobject


