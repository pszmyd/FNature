namespace FGene.Population

open FGene.Chromosomes

/// Describes chromosome population
type Population<'T when 'T : comparison>(?count : int, ?size : int) =
    let mutable population = ChromosomeFactory.randomize<'T> count size
    member this.Chromosomes with get() = population
                            and set(value) = population <- value
    member this.Size with get() = size

