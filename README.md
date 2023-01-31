# LabelledData [![Build Status](https://github.com/emmt/LabelledData.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/LabelledData.jl/actions/workflows/CI.yml?query=branch%3Amain) [![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/LabelledData.jl?svg=true)](https://ci.appveyor.com/project/emmt/LabelledData-jl) [![Coverage](https://codecov.io/gh/emmt/LabelledData.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/LabelledData.jl)

Labelled vectors are [Julia](https://julialang.org) vectors whose entries have
labelspu. Labelled vectors can be efficiently indexed by linear indices
(integers) as ordinary vectors but also by labels. Labels may be anything but
integers and need not be unique unlike dictionaries, *ordered dictionaries*
provided by the
[`DataStructures`](https://github.com/JuliaCollections/DataStructures.jl)
package, or *named vectors* provided by the
[`Named`](https://github.com/HarlanH/Named.jl) package. Compared to `LArray`
and `SLArray` provided by the
[`LabelledArrays`](https://github.com/SciML/LabelledArrays.jl) package, labels
are not immutable.

The `LabelledData` package provides two kinds of labelled vectors: those whose
labels and values are stored separately and those whose labels can be deduced
from the values and which only require to store these values.


## Labelled vectors with given labels

To build a labelled vector for which labels and values must be both specified,
call:

``` julia
A = LabelledVector{L}(labs::AbstractVector{<:L}, vals::AbstractVector{V})
```

where `labs` and `vals` are two vectors respectively storing the labels and the
values of the initial entries of the labelled vector. The labelled vector `A`
is built on top of `labs` and `vals` which must be not changed by other means
than the labelled vector API. If that is not possible, provide copies instead.

Specifying `L` is useful to widen the class of allowed labels when
indexing/searching a labelled vector by label. For example:

``` julia
A = LabelledVector{AbstractString}(["a","b"],[1,2])
```

yields a labelled vector indexable and searchable by any sub-type of
`AbstractString` even though labels are stored as `String`. `L = eltype(labs)`
is assumed if type parameter `L` is not specified.

Calling:

``` julia
push!(A, lab => val)
```

appends a *new entry* labelled `lab` and with value `val` whether an entry with
the same label already exists in `A` or not.

In addition to standard linear indexing by 1-based integers, the syntax:

``` julia
A[lab::L] -> val::V
```

yields the *first entry* in `A` whose label is `lab`. Setting entries by label
is also implemented:

``` julia
A[lab::L] = val
```

sets the value of the *first entry* in `A` whose label is `lab` or, if no such
entry exists, appends `lab => val` as a *new entry* as if `push!(A,lab=>val)`
has been called.


## Labelled vectors with computed labels

A function that computes the label of an entry given its value, say:

``` julia
getlabel(val::V) -> lab::L
```

can be specified instead of a vector of labels:

``` julia
A = LabelledVector{L}(getlabel::Function, vals::AbstractVector{V})
```

to build a labelled vector whose entry labels can be determined from the values
of the entries. the type `L` of allowed labels must be specified in curly
braces for this kind of labelled vector.

With such a labelled vector, calling:

``` julia
push!(A, val)
```

appends a new entry with value `val` whether an entry with the label
`getlabel(val)` already exists in `A` or not.

In addition to standard linear indexing by 1-based integers, the syntax:

``` julia
A[lab::L] -> val::V
```

yields the *first entry* in `A` whose label is `lab`. Setting entries by label
is also implemented:

``` julia
A[] = val
```

sets the value of the *first entry* in `A` whose label is `getlabel(val)` or, if
no such entry exists, appends `val` as a *new entry* as if `push!(A, val)` has
been called.


## Searching all entries with a given label

Since entry labels may not be unique, standard methods `findfirst`, `findnext`,
`findlast`, and `findprev` can be used to find all entries matching a given
label. For example, to iterate over all entries of a labelled vector `A` in
increasing order of their indices, do:

``` julia
i = findfirst(lab, A)
@inbounds while i !== nothing
    val = A[i]
    ... # do something
    i = findnext(lab, A, i+1)
end
```

This can be shortened by using the `eachmatch` method which returns an
iterator over the values that match a given label:

``` julia
for val in eachmatch(lab, A, Forward)
    ... # do something
end
```

As you can guess from the above examples, iterating in reverse order is
possible:

``` julia
for val in eachmatch(lab, A, Reverse)
    ... # do something
end
```

which is equivalent to:

``` julia
i = findlast(lab, A)
@inbounds while i !== nothing
    val = A[i]
    ... # do something
    i = findprev(lab, A, i-1)
end
```

and also to:

``` julia
for val in reverse(eachmatch(lab, A, Forward))
    ... # do something
end
```

Using `eachmatch` is generally a bit faster than using `findfirst` and
`findnext` or `findlast` and `findprev`. If the order of values does not
matter, it is faster to not specify an ordering (or to specify `Unorderd`
instead of `Forward` or `Reverse`):

``` julia
eachmatch(lab, A)
eachmatch(lab, A, Unordered)
```

To retrieve all values with a given label, say `lab`, in unspecific order, do:

``` julia
collect(lab, A)
```

which can be seen as a shortcut for:

``` julia
collect(eachmatch(lab, A))
```


A third argument may be specified to retrieve the values in a given order:

``` julia
collect(lab, A, Forward)  # all values labelled by `lab` in order of increasing indices
collect(lab, A, Reverse)  # all values labelled by `lab` in order of deccreasing indices
collect(lab, A, Unorderd) # all values labelled by `lab` in unspecific order
```

Counting the number of entries with a given label is done by one of the
following equally efficient statements:

``` julia
count(lab, A)
length(eachmatch(lab, A))
```


## Using integers as labels

To use integers as labels, a possible trick is to wrap the integer value in a
simple structure and extend the base method `convert`. For example:

``` julia
struct Label
    lab::UInt16
end
Label(x::Label) = x
Base.convert(::Type{Label}, x::Label) = x
Base.convert(::Type{Label}, x::Integer) = Label(x)
A = LabelledVector(Label[], Float64[])
```

yields an empty labelled vector whose entries have type `Float64` and labels
have type `UInt16` wrapped in a `Label` structure.
