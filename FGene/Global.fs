namespace FGene.Global

module Helpers = 
    open System

    let equalsOn f x (yobj:obj) =
        match yobj with
        | :? 'T as y -> (f x = f y)
        | _ -> false
 
    let hashOn f x =  hash (f x)
 
    let compareOn f x (yobj: obj) =
        match yobj with
        | :? 'T as y -> compare (f x) (f y)
        | _ -> invalidArg "yobj" "cannot compare values of different types"

    //https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs
    type NullCoalesce =  
        static member Coalesce(a: 'a option, b: 'a Lazy) = match a with Some a -> a | _ -> b.Value
        static member Coalesce(a: 'a Nullable, b: 'a Lazy) = if a.HasValue then a.Value else b.Value
        static member Coalesce(a: 'a when 'a:null, b: 'a Lazy) = match a with null -> b.Value | _ -> a

    let inline nullCoalesceHelper< ^t, ^a, ^b, ^c when (^t or ^a) : (static member Coalesce : ^a * ^b -> ^c)> a b = 
                                                ((^t or ^a) : (static member Coalesce : ^a * ^b -> ^c) (a, b))

    let inline (|??) a b = nullCoalesceHelper<NullCoalesce, _, _, _> a b

    let inline average list = (List.fold (fun acc elem -> acc + float elem) 0.0 list / float list.Length)

    // The following example computes the standard deviation of a list.
    // The standard deviation is computed by taking the square root of the
    // sum of the variances, which are the differences between each value
    // and the average.
    let inline stdDev list =
        let avg = average list
        sqrt (List.fold (fun acc elem -> acc + (float elem - avg) ** 2.0 ) 0.0 list / float list.Length)

module Seq = 
    let pair s =
        s |> Seq.pairwise 
          |> Seq.mapi (fun i x -> i % 2 = 0, x) 
          |> Seq.filter fst 
          |> Seq.map snd

module Random =
    open FizzWare.NBuilder

    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    let charsLen = chars.Length

    let rng = System.Random()
    let g = RandomGenerator()

    let randomString len = 
        let randomChars = [|for i in 0..len -> chars.[rng.Next(charsLen)]|]
        System.String(randomChars)

    let inline randomChar() = chars.[rng.Next(charsLen)]


