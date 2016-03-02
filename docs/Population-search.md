# Population-based search

For all of the following approaches, we chose to combine the old and new
generations and take the best half of the population to form the next
generation. This process is implemented in the whimsically named
`thin-the-herd`.


## Mutation Method

The simple mutation strategy just takes in a mutation and maps it across
the entire population. Currently, the only mutation that we have defined
is from the previous lab.

-   `flip-one-bit`: Given an answer, flip-one-bit modifies the choices]
    array by randomly flipping a single bit.


## Crossover Methods

Our crossover methods both function by selecting two random parents to
build new children until the population size is doubled. Then we
`thin-the herd`...

#### Uniform crossover

The uniform crossover strategy, when building a child, selects values
at random from the respective positions of both parents' choice strings.

#### N-point crossover
