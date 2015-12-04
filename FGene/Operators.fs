namespace FGene.Global

open FGene.Gene
open FGene.Chromosomes

module Operators =

    /// Creates a deep copy of a given chromosome
    let spawn (x : 'T when 'T :> IChromosome<'U>) = 
        let newX = match x.Mutator with
                   | Some m -> new Chromosome<'U>(m) :> IChromosome<'U>
                   | None -> new Chromosome<'U>() :> IChromosome<'U>

        newX.Contents <- (x.Contents |> Array.copy)
        newX

    /// Array element exchange operator
    let inline (<->) (x : ^T array, y : ^T array) (index : int) = 
        let tmp = x.[index]
        x.[index] <- y.[index]
        y.[index] <- tmp
        (x, y)

    /// Single-point crossover operator
    let inline (<|>) (x : ^T when ^T :> IChromosome<'U>) (y : ^T) =
        let splice = Random.rng.Next(0, x.Contents.Length-1)

        let xx = spawn x
        let yy = spawn y

        for i in 0..splice do (xx.Contents, yy.Contents) <-> i |> ignore

        (xx, yy)

    /// Double-point crossover operator
    let inline (<||>) (x : ^T when ^T :> IChromosome<'U>) (y : ^T) =
        let spliceStart = Random.rng.Next(0, x.Contents.Length-1)
        let spliceEnd = Random.rng.Next(spliceStart+1, x.Contents.Length-1)

        let xx = spawn x
        let yy = spawn y

        for i in spliceStart..spliceEnd do (xx.Contents, yy.Contents) <-> i |> ignore

        (xx, yy)

    /// Mutation operator
    let inline (<~~) (x : ^T when ^T :> IChromosome<'U>) (chance : float) = 
        // Set specified elements to new, mutated values
        x.Contents 
        |> Seq.iteri(fun i g -> if Random.rng.NextDouble() < chance then x.Contents.[i] <- g.Mutate())

        x


