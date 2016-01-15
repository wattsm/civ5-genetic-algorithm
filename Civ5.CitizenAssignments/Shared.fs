///Contains shared utility functions
[<AutoOpen>]
module Shared

///Contains functions for generating random numbers and related values
[<RequireQualifiedAccess>]
module RNG = 

    open MathNet.Numerics.Random

    ///Thread safe random number generator
    let private random = SystemRandomSource.Default

    ///Get a value between min and max, inclusive
    let get min max = 
        random.Next (min, (max + 1))    

    ///True if a random value is greater than or equal to a given threshold
    let threshold min max value = 
        (get min max) >= value

    ///True if a random value is within a given percentile rate (e.g. 0.85 will be true 85% of the time)
    let rate value =             
        let value' = int ((1m - value) * 100m) //Convert 0.85 to a threshold of 25
        in threshold 1 100 value'

    ///True 50% of the time
    let flip () = rate 0.5M

///Contains functions for use with the Option<T> type
[<RequireQualifiedAccess>]
module Option = 

    ///Convert from Option<T> to T using the given default value for None
    let unwrap defaultValue = function
        | Some value -> value
        | _ -> defaultValue

///Contains functions for use with the List<T> type
[<RequireQualifiedAccess>]
module List = 

    ///Remove an item by index
    let removeIndex index = 
        let rec f current = function
            | [] -> []
            | item::items ->
                if (current = index) then
                    f (current + 1) items
                else
                    item :: (f (current + 1) items)
        in f 0                

    ///Detach the head from a list as a tuple
    let detach items = 
        List.head items,
        List.tail items

    ///Select a random item from a list
    let selectOne list = 
        let max = (List.length list) - 1
        in List.nth list (RNG.get 0 max)

    ///Select a given number of random items from a list
    let rec selectMany count list = 
        if (count = 0) then
            []
        else

            let max = (List.length list) - 1
            in let index = RNG.get 0 max

            let picked = List.nth list index
            let remaining = removeIndex index list
            
            picked :: (selectMany (count - 1) remaining)

    ///Indexes a list, converting from 'a list to (Int32 * 'a) list
    let index items = List.mapi (fun index item -> (index, item)) items