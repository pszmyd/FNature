namespace FGene.Gene

open System
open FGene.Global.Random
open FGene.Global.Helpers

[<Interface>]
type IGene<'T when 'T : comparison> =
    /// Data associated with this particular gene.
    abstract member Contents : 'T with get

    /// Returns a mutated version of the current object
    abstract member Mutate<'T> : unit -> IGene<'T>

    inherit IComparable
    inherit IComparable<IGene<'T>>
    inherit IEquatable<IGene<'T>>

/// Represents a gene object of which chromosomes are composed
[<StructAttribute>]
[<CustomEquality>]
[<CustomComparison>]
type Gene<'T when 'T : equality and 
                  'T : comparison> = 

    val private _contents : 'T
    val private _mutator : option<'T -> 'T>
    val private _typedef : Type

    new(v : 'T, ?mutator : 'T -> 'T) = { _contents = v; _mutator = mutator; _typedef = typedefof<'T> }

    interface IGene<'T> with
        member this.Contents with get() = this._contents
        member this.Mutate() = 
            let newVal = 
                match this._mutator with 
                | None -> unbox(match this._typedef with     
                                | x when x = typedefof<byte> -> box(byte(rng.Next(0, 256)))   
                                | x when x = typedefof<string> -> 
                                    box(match ((this._contents :> obj) :?> string) with
                                        | null -> randomString 5
                                        | x -> randomString x.Length)
                                | x when x = typedefof<bool> -> box(g.Boolean())
                                | x when x = typedefof<int> -> box(rng.Next())
                                | x when x = typedefof<int64> -> box(g.Long())
                                | x when x = typedefof<char> -> box(randomChar())
                                | x when x = typedefof<DateTime> -> box(g.DateTime())
                                | x when x = typedefof<Decimal> -> box(g.Decimal())
                                | x when x = typedefof<float> -> box(g.Float())
                                | x when x = typedefof<Guid> -> box(Guid.NewGuid())
                                | _ -> box(this._contents))
                | Some fn -> fn this._contents

            if newVal <> this._contents then
                //printfn "Switched %s to %s" (this._contents.ToString()) (newVal.ToString())
                match this._mutator with
                | Some m -> Gene<'T>(newVal, m) :> IGene<'T>
                | None -> Gene<'T>(newVal) :> IGene<'T>
            else
                this :> IGene<'T>

    interface IEquatable<IGene<'T>> with
        member this.Equals x = this._contents.Equals(x.Contents)

    override this.Equals(yobj) =
        match yobj with
        | :? IGene<'T> as x -> this._contents.Equals(x.Contents)
        | _ -> false
 
    override this.GetHashCode() = hash this._contents

    interface System.IComparable with
      member this.CompareTo yobj =
          match yobj with
          | :? IGene<'T> as y -> compare (this :> IGene<'T>).Contents y.Contents
          | _ -> invalidArg "yobj" "cannot compare values of different types"

    interface System.IComparable<'T IGene> with
      member this.CompareTo y =
          compare (this :> IGene<'T>).Contents y.Contents

    override this.ToString() = 
        String.Format("{0}", (this :> IGene<'T>).Contents)

module Operators = 
    /// Shorthand for creating a gene from an arbitrary value
    let gene (x : 'T) (mutator : option<'T -> 'T>) = 
        match mutator with
        | Some m -> Gene<'T>(x, m) :> IGene<'T>
        | None -> Gene<'T>(x) :> IGene<'T>

    /// Helper for fetching gene contents
    let (!) (x : #IGene<'T>) = x.Contents

