namespace FGene.Selection

open FGene.Global
open FGene.Population

module Algorithms =

    open FGene.Chromosomes

    let private sus (number : int) (population : Population<'T>) =    
        let chromosomes = population.Chromosomes |> Seq.toList
        let numberToPick = if number = 0 then chromosomes.Length else number

        let sumFitness = chromosomes |> Seq.sumBy(fun c -> c.Fitness)
        let stepSize = 1.0 / double numberToPick;

        let accumulativePercent = ref 0.0;
        let rouletteWheel = 
            chromosomes 
            |> Seq.map(fun c -> accumulativePercent := !accumulativePercent + (c.Fitness  / sumFitness)
                                !accumulativePercent)
            |> Seq.toArray

        let p = ref(Random.rng.NextDouble())
        seq { 
            for _ in 1..numberToPick do       
                let pointer = match !p with 
                              | x when x > double 1.0 -> x - double 1.0 
                              | x -> x

                let chromosomeIndex = 
                    rouletteWheel
                    |> Seq.mapi(fun index value -> (value, index))
                    |> Seq.tryFind(fun (value, _) -> value >= pointer)

                if chromosomeIndex.IsSome then
                    yield chromosomes.[snd chromosomeIndex.Value]
                else
                    yield chromosomes.[Random.rng.Next(0, chromosomes.Length)]

                p := !p + stepSize;                
        } 

    let private randomize (population : Population<'T>) = 
        let arr = population.Chromosomes |> Seq.toArray
        arr |> Array.map(fun _ -> arr.[Random.rng.Next(arr.Length)]) :> seq<IChromosome<'T>>

    /// List of available selection algorithms
    type public Algorithm = 
        | StochasticUniform of int
        | Random
    
    /// Applies a given algorithm over a given chromosome population, resulting with a new chromosome collection    
    let apply population algorithm = 
        match algorithm with
        | StochasticUniform(number) -> sus number population
        | Random -> randomize population


