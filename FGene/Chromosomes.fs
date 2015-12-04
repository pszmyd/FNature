namespace FGene.Chromosomes

open FGene.Global
open FGene.Gene
open FGene.Gene.Operators

/// Base interface for all chromosomes.
[<Interface>]
type IChromosome = 
   abstract member Fitness : float with get, set
   abstract member Randomize : int -> unit

// Base interface for chromosome classes.
[<Interface>]
type IChromosome<'T when 'T : comparison> =
   inherit IChromosome
   abstract member Contents : IGene<'T> array with get, set
   abstract member Mutator : option<'T -> 'T>

type Chromosome<'T when 'T : comparison>(?mutator : 'T -> 'T) = 
    let mutable data = [||]
    let mutable fitness = 0.0

    interface IChromosome<'T> with
        member this.Randomize(size : int) = 
            data <- [| for _ in 1 .. size -> (gene Unchecked.defaultof<'T> mutator).Mutate() |]
        member this.Contents with get () = data
                             and set(value) = data <- value
        member this.Fitness with get() = fitness
                            and set(value) = fitness <- value
        member this.Mutator = mutator

    override this.ToString() = 
        System.String.Format("F:{0,8:0.000}\t{1}", fitness, (this :> IChromosome<'T>).Contents |> Seq.map(fun x -> x.ToString()) |> String.concat "|")

module ChromosomeFactory = 
    let randomize<'T when 'T : comparison> (count : option<int>) (size : option<int>) = 
        let c = match count with 
                | Some x when x > 0 -> x 
                | Some x when x <= 0 -> 0 
                | None -> 100 
                | _ -> 100

        let s = match size with | Some x -> x | None -> 10

        [
            for _ in 1..c do 
                let chr = Chromosome<'T>() :> IChromosome<'T>
                chr.Randomize(s)
                yield chr
        ]
                