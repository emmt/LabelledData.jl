module LabelledData

export
    LabelledVector,
    Forward,
    Reverse,
    Unordered

using Base: @propagate_inbounds

using Base.Order: Ordering, Forward, Reverse
struct UnspecificOrdering <: Ordering end
const Unordered = UnspecificOrdering()

"""
    LabelledVector{L}(labs::AbstractVector{<:L}, vals::AbstractVector{V}) -> A

builds a vector of labelled entries on top of vectors `labs` and `vals`
repectively storing the labels and values of the initial entries. Labelled
vectors behave as vectors of entries of type `V` but can also be efficiently
indexed by labels of type `L`. Compared to a dictionary, entries in a labelled
vector keep their order and different entries may have the same label.

Specifying `L` is useful to widen the class of allowed labels when indexing or
searching a labelled vector by label. For example:

    A = LabelledVector{AbstractString}(["a","b"],[1,2])

yields a labelled vector indexable and searchable by any sub-type of
`AbstractString` even though labels are stored as `String`. `L = eltype(labs)`
is assumed if type parameter `L` is not specified.

Calling:

    push!(A, lab => val)

appends a *new entry* labelled `lab` and with value `val` whether an entry with
the same label already exists in `A` or not.

In addition to standard linear indexing by 1-based integers, the syntax:

    A[lab::L] -> val::V

yields the *first entry* in `A` whose label is `lab`. Setting entries by label
is also implemented:

    A[lab::L] = val

sets the value of the *first entry* in `A` whose label is `lab` or, if no such
entry exists, appends `lab => val` as a *new entry* as if `push!(A,lab=>val)`
has been called.

Standard methods `findfirst`, `findnext`, `findlast`, and `findprev` can be
used to find all entries matching a given label. For example:

    i = findfirst(lab, A)
    while i !== nothing
        val = A[i]
        ... # do something with i-th entry
        i = findnext(lab, A, i)
    end

---

    LabelledVector{L}(getlabel::Function, vals::AbstractVector{V}) -> A

builds a labelled vector of entries on top of `vals` the vector storing the
initial entries. Argument `getlabel` is a callable object which is used to
retrieve the *label* of an entry of type `V`:

    getlabel(val::V) -> lab::L

should hold.

Calling:

    push!(A, val)

appends a *new entry* with value `val` whether an entry with the same label
already exists in `A` or not.

In addition to standard linear indexing by 1-based integers, the syntax:

    A[lab::L] -> val::V

yields the *first entry* in `A` whose label is `lab`. Setting entries by label
is also implemented:

    A[] = val

sets the value of the *first entry* in `A` whose label is `getlabel(val)` or,
if no such entry exists, appends `val` as a *new entry* as if `push!(A, val)`
has been called.

"""
struct LabelledVector{L,V,
                      # NOTE: It is not possible to be more specific about the
                      # type of labs, Q<:Union{AbstractVector{L},Function}
                      # works but Q<:Union{AbstractVector{<:L},Function}
                      # doesn't. Since the latter is what we want, we have to
                      # relax the type of Q and explicitly check for the
                      # constraint in the constructors.
                      Q<:Union{AbstractVector,Function},
                      R<:AbstractVector{V}} <: AbstractVector{V}
    labs::Q            # vector of labels or function to retrieve label from value
    vals::R            # vector of values
    keys::Vector{UInt} # hash keys of values
    first::Vector{Int} # indices of first entry in each slot
    next::Vector{Int}  # indices of next entry in same slot, 0 if last of slot
    function LabelledVector{L,V,Q,R}(labs::Q,
                                     vals::R) where {L,V,
                                                     Q<:Union{AbstractVector,
                                                              Function},
                                                     R<:AbstractVector{V}}
        L <: Integer && throw(ArgumentError("labels must not be integers"))
        labs isa AbstractVector && !(eltype(labs) <: L) && throw(ArgumentError(
            "incompatible vector of type `$Q` for storing labels of type `$L`"))
        Base.has_offset_axes(vals) && throw(ArgumentError(
            "vector of values must have 1-based indices"))
        IndexStyle(vals) === IndexLinear() || throw(ArgumentError(
            "vector of values must have linear indices"))
        len = length(vals)
        if Q <: AbstractVector
            Base.has_offset_axes(labs) && throw(ArgumentError(
                "vector of labels must have 1-based indices"))
            IndexStyle(labs) === IndexLinear() || throw(ArgumentError(
                "vector of labels must have linear indices"))
            length(labs) == len || throw(DimensionMismatch(
                "vectors of labels and of values must have the same size"))
        end
        first = Vector{Int}(undef, slot_size(len))
        @inbounds for i in eachindex(first)
            first[i] = 0 # NOTE: firstindex(vals) - 1
        end
        keys = Vector{UInt}(undef, len)
        next = Vector{Int}(undef, len)
        @inbounds for i in Base.OneTo(len)
            # Insert initial values, one by one, in the slots.
            key = hash_label(L, labs, vals, i)
            slot = circular_index(first, key)
            keys[i] = key
            next[i] = first[slot]
            first[slot] = i
        end
        return new{L,V,Q,R}(labs, vals, keys, first, next)
    end
end

# Constructors.
LabelledVector(labs::AbstractVector, vals::AbstractVector) =
    LabelledVector{eltype(labs)}(labs, vals)

LabelledVector{L}(labs::Union{AbstractVector,Function}, vals::AbstractVector) where {L} =
    LabelledVector{L,eltype(vals),typeof(labs),typeof(vals)}(labs, vals)

# Helper function for constructor.
@inline @propagate_inbounds function hash_label(::Type{L}, getlabel::Function,
                                                vals::AbstractVector, i::Int) where {L}
    return hash(getlabel(vals[i])::L)
end

@inline @propagate_inbounds function hash_label(::Type{L}, labs::AbstractVector{<:L},
                                                vals::AbstractVector, i::Int) where {L}
    return hash(labs[i])
end

# Implement abstract array API.
Base.IndexStyle(::Type{<:LabelledVector}) = IndexLinear()
Base.length(A::LabelledVector) = length(A.vals)
Base.size(A::LabelledVector) = (length(A),)
Base.axes(A::LabelledVector) = (keys(A),)
Base.keys(A::LabelledVector) = Base.OneTo(length(A))
Base.firstindex(A::LabelledVector) = 1
Base.lastindex(A::LabelledVector) = length(A)

@inline function Base.getindex(A::LabelledVector, i::Int)
    vals = A.vals
    @boundscheck checkbounds(vals, i)
    @inbounds vals[i]
end

function Base.getindex(A::LabelledVector{L}, lab::L) where {L}
    i = findfirst(lab, A)
    i === nothing && throw(KeyError(lab))
    @inbounds A[i]
end

function Base.get(A::LabelledVector{L}, i::Integer, def) where {L}
    i = Int(i)::Int
    checkbounds(Bool, A, i) ? def : @inbounds A[i]
end

function Base.get(A::LabelledVector{L}, lab::L, def) where {L}
    i = findfirst(lab, A)
    i === nothing ? def : @inbounds A[i]
end

@inline Base.setindex!(A::LabelledVector{L,V,<:Function}, val, i::Int) where {L,V} =
    setindex!(A, convert(V, val)::V, i)

@inline function Base.setindex!(A::LabelledVector{L,V,<:Function},
                                val::V, i::Int) where {L,V}
    @boundscheck checkbounds(A, i)
    @inbounds begin
        change_key(A, i, hash(A.labs(val)::L))
        A.vals[i] = val
    end
    return A
end

@inline @propagate_inbounds function Base.setindex!(A::LabelledVector{<:Any,<:Any,
                                                                      <:AbstractVector{L},
                                                                      <:AbstractVector{V}},
                                                    pair::Pair, i::Int) where {L,V}
    return setindex!(A, convert(L, pair.first)::L => convert(V, pair.second)::V, i)
end

@inline function Base.setindex!(A::LabelledVector{<:Any,<:Any,
                                               <:AbstractVector{L},
                                               <:AbstractVector{V}},
                                pair::Pair{L,V}, i::Int) where {L,V}
    @boundscheck checkbounds(A, i)
    @inbounds begin
        lab, val = pair
        change_key(A, i, hash(lab))
        A.labs[i] = lab
        A.vals[i] = val
     end
    return A
end

@inline @propagate_inbounds function change_key(A::LabelledVector, i::Int,
                                                new_key::UInt)
    old_key = get_key(A, i)
    if new_key != old_key
        old_slot = circular_index(A.first, old_key)
        new_slot = circular_index(A.first, new_key)
        if new_slot != old_slot
            A.first[old_slot] = A.next[i]
            A.next[i] = A.first[new_slot]
            A.first[new_slot] = i
        end
    end
end

@inline @propagate_inbounds function Base.setindex!(A::LabelledVector{L,V,<:Function},
                                                    val) where {L,V}
    return setindex!(A, convert(V, val)::V, i)
end

function Base.setindex!(A::LabelledVector{L,V,<:Function}, val::V) where {L,V}
    key = hash(A.labs(val)::L)
    slot = circular_index(A.first, key) # FIXME: get_slot(A, key)?
    j = A.first[slot]
    @inbounds while j > 0
        if get_key(A, j) == key && get_label(A, j) == lab
            # Replace first existing entry with same label.
            A.vals[j] = val
            return A
        end
    end
    # No existing entries with same label, insert as new entry (like push!).
    push!(A.keys, key)
    push!(A.next, A.first[slot])
    @inbounds A.first[slot] = lastindex(push!(A.vals, val))
    return A
end

@inline function get_label(A::LabelledVector{<:Any,<:Any,<:AbstractVector}, i::Int)
    @boundscheck checkbounds(A.labs, i)
    @inbounds A.labs[i]
end

@inline function get_label(A::LabelledVector{<:Any,<:Any,<:Function}, i::Int)
    @boundscheck checkbounds(A.vals, i)
    A.labs(@inbounds A.vals[i])
end

@inline function get_key(A::LabelledVector, i::Int)
    @boundscheck checkbounds(A.keys, i)
    @inbounds A.keys[i]
end

@inline function get_next(A::LabelledVector, i::Int)
    @boundscheck checkbounds(A.next, i)
    @inbounds A.next[i]
end

get_first(A::LabelledVector, key::UInt) =
    @inbounds A.first[circular_index(A.first, key)]

circular_index(A::AbstractVector, key::UInt) =
    Int(mod(key, length(A)%UInt)) + firstindex(A)

Base.push!(A::LabelledVector{L,V,<:Function}, val) where {L,V} =
    push!(A, convert(V, val)::V)

function Base.push!(A::LabelledVector{L,V,<:Function}, val::V) where {L,V}
    if length(A) ≥ rehash_size(A)
        rehash!(A, +1)
    end
    key = hash(A.labs(val)::L)
    i = circular_index(A.first, key)
    push!(A.keys, key)
    push!(A.next, A.first[i])
    @inbounds A.first[i] = lastindex(push!(A.vals, val))
    return A
end

# First convert, then push.
function Base.push!(A::LabelledVector{<:Any,<:Any,
                                   <:AbstractVector{L},
                                   <:AbstractVector{V}},
                    pair::Pair) where {L,V}
    return push!(A, convert(L, pair.first)::L => convert(V, pair.second)::V)
end

function Base.push!(A::LabelledVector{<:Any,<:Any,
                                   <:AbstractVector{L},
                                   <:AbstractVector{V}},
                    pair::Pair{L,V}) where {L,V}
    if length(A) ≥ rehash_size(A)
        rehash!(A, +1)
    end
    lab, val = pair
    key = hash(lab)
    i = circular_index(A.first, key)
    push!(A.keys, key)
    push!(A.next, A.first[i])
    push!(A.labs, lab)
    @inbounds A.first[i] = lastindex(push!(A.vals, val))
    return A
end

const MIN_SIZE = 16
const GROWTH_RATE = 3//2 # NOTE: must be > 1

# slot_size//data_size ratio
const TRESHOLD_RATIO = 5//3

rehash_size(A::LabelledVector) = rehash_size(length(A.first))
rehash_size(len::Int) =
    round(Int, (len*denominator(TRESHOLD_RATIO))//numerator(TRESHOLD_RATIO))

function slot_size(len::Int)
    size = MIN_SIZE
    if len > rehash_size(len)
        min_size = round(Int, TRESHOLD_RATIO*len)
        while size < min_size
            size = round(Int, size*GROWTH_RATE)
        end
    end
    return size
end

function rehash!(A::LabelledVector, adj::Int)
    old_len = length(A)
    new_len = slot_size(old_len + max(adj, 0))
    if new_len > old_len
        # Resize slot and set new part to end mark.
        resize!(A.first, new_len)
        @inbounds for i in old_len+1:new_len
            A.first[i] = 0
        end
        # Rehash old slot part.
        @inbounds for i in 1:old_len
            j = A.first[i]
            ip = circular_index(A.first, get_key(A, j))
            if ip != i
                # Remove entry from its slot and re-insert it in the right slot.
                A.first[i] = A.next[j]
                A.next[j] = A.first[ip]
                A.first[ip] = j
            end
        end
    end
    return A
end

# Yield whether i-th entry matches a given label (and its hash key).
@inline function is_matching(A::LabelledVector{L}, i::Int, lab::L, key::UInt) where {L}
    @boundscheck checkbounds(A, i)
    # NOTE First check key as it is cheap, then check label.
    return @inbounds (get_key(A, i) === key && get_label(A, i) == lab)
end

function Base.findfirst(lab::L, A::LabelledVector{L}) where {L}
    first_index, last_index = firstindex(A), lastindex(A)
    key = hash(lab)
    j = get_first(A, key)
    k = last_index + 1
    @inbounds while j > 0
        if j < k && is_matching(A, j, lab, key)
            k = j
        end
        j = get_next(A, j)
    end
    return k ≤ last_index ? k : nothing
end

function Base.findlast(lab::L, A::LabelledVector{L}) where {L}
    first_index = firstindex(A)
    key = hash(lab)
    j = get_first(A, key)
    k = first_index - 1
    @inbounds while j > 0
        if j > k && is_matching(A, j, lab, key)
            k = j
        end
        j = get_next(A, j)
    end
    return k ≥ first_index ? k : nothing
end

Base.findnext(lab::L, A::LabelledVector{L}, i::Int) where {L} = _findnext(A, i, lab)
Base.findprev(lab::L, A::LabelledVector{L}, i::Int) where {L} = _findprev(A, i, lab)

@inline function _findnext(A::LabelledVector{L}, i::Int, lab::L,
                           _key::Union{UInt,Nothing} = nothing) where {L}
    first_index, last_index = firstindex(A), lastindex(A)
    i > last_index && return nothing
    i < first_index && throw(Boundserror(A, i))
    key = (_key === nothing ? hash(lab) : _key)
    j = get_first(A, key)
    k = last_index + 1
    @inbounds while j > 0
        if i ≤ j < k && is_matching(A, j, lab, key)
            k = j
        end
        j = get_next(A, j)
    end
    return k ≤ last_index ? k : nothing
end

@inline function _findprev(A::LabelledVector{L}, i::Int, lab::L,
                           _key::Union{UInt,Nothing} = nothing) where {L}
    first_index, last_index = firstindex(A), lastindex(A)
    i < first_index && return nothing
    i > last_index && throw(Boundserror(A, i))
    key = (_key === nothing ? hash(lab) : _key)
    j = get_first(A, key)
    k = first_index - 1
    @inbounds while j > 0
        if i ≥ j > k && is_matching(A, j, lab, key)
            k = j
        end
        j = get_next(A, j)
    end
    return k ≥ first_index ? k : nothing
end

Base.findfirst(pred::Function, A::LabelledVector) = unsafe_find(pred, A, keys(A))
Base.findlast(pred::Function, A::LabelledVector) = unsafe_find(pred, A, reverse(keys(A)))
function Base.findnext(pred::Function, A::LabelledVector, i::Int)
    i < firstindex(A) && throw(Boundserror(A, i))
    return unsafe_find(pred, A, i : lastindex(A))
end
function Base.findprev(pred::Function, A::LabelledVector, i::Int)
    i > lastindex(A) && throw(Boundserror(A, i))
    return unsafe_find(pred, A, i : -1 : firstindex(A))
end

@inline function unsafe_find(pred::Function, A::LabelledVector, I::OrdinalRange{Int})
    @inbounds for i in I
        pred(A[i]) && return i
    end
    return nothing
end

#------------------------------------------------------------------------------
# Iterator over labelled entries, first type parameter indicates whether order
# matters.
struct LabelledIterator{O<:Ordering,A<:LabelledVector,L}
    obj::A
    lab::L
    key::UInt
end

get_first(iter::LabelledIterator) = get_first(iter.obj, iter.key)
@inline @propagate_inbounds get_val(iter::LabelledIterator, i::Int) = iter.obj[i]
@inline @propagate_inbounds get_next(iter::LabelledIterator, i::Int) = get_next(iter.obj, i)
@inline @propagate_inbounds is_matching(iter::LabelledIterator, i::Int) =
    is_matching(iter.obj, i, iter.lab, iter.key)

"""
    count(lab::L, A::LabelledVector{L})

yield the  number of entries in `A` whose label is `lab`.

"""
Base.count(lab::L, A::LabelledVector{L}) where {L} = unsafe_count(A, lab)

@inline function unsafe_count(A::LabelledVector{L}, lab::L, key::UInt = hash(lab),
                              i::Int = get_first(A, key)) where {L}
    n = 0
    @inbounds while i > 0
        if is_matching(A, i, lab, key)
            n += 1
        end
        i = get_next(A, i)
    end
    return n
end

Base.IteratorEltype(::Type{<:LabelledIterator}) = Base.HasEltype()
Base.eltype(::Type{<:LabelledIterator{O,A,L}}) where {O,A,L} = eltype(A)

Base.IteratorSize(::Type{<:LabelledIterator}) = Base.HasLength()
Base.length(iter::LabelledIterator) = unsafe_count(iter.obj, iter.lab, iter.key)

Base.reverse(iter::LabelledIterator{typeof(Forward)}) =
    LabelledIterator(Reverse, iter.obj, iter.lab, iter.key)
Base.reverse(iter::LabelledIterator{typeof(Reverse)}) =
    LabelledIterator(Forward, iter.obj, iter.lab, iter.key)
Base.reverse(iter::LabelledIterator{typeof(Unordered)}) = iter

LabelledIterator(order::O, A::LabelledVector{L}, lab::L, key::UInt) where {O<:Ordering,L} =
    LabelledIterator{O,typeof(A),L}(A, lab, key)
LabelledIterator(order::Ordering, A::LabelledVector{L}, lab::L) where {L} =
    LabelledIterator(order, A, lab, hash(lab))

# Iterate over entries in unspecifiec order.
function Base.iterate(iter::LabelledIterator{typeof(Unordered)},
                      i::Int = get_first(iter))
    if firstindex(iter.obj) ≤ i ≤ lastindex(iter.obj)
        @inbounds while i > 0
            i_next = get_next(iter, i)
            if is_matching(iter, i)
                return (get_val(iter, i), i_next)
            end
            i = i_next
        end
    end
    return nothing
end

# Iterate over entries in forward order.
function Base.iterate(iter::LabelledIterator{typeof(Forward)},
                      i::Int = firstindex(iter.obj))
    j = _findnext(iter.obj, i, iter.lab, iter.key)
    return j === nothing ? nothing : (@inbounds get_val(iter, j), j+1)
end

# Iterate over entries in backward order.
function Base.iterate(iter::LabelledIterator{typeof(Reverse)},
                      i::Int = lastindex(iter.obj))
    j = _findprev(iter.obj, i, iter.lab, iter.key)
    return j === nothing ? nothing : (@inbounds get_val(iter, j), j-1)
end

"""
    eachmatch(lab::L, A::LabelledVector{L,V}, order::Ordering = Unordered)

yields an iterator over the values of `A` labelled by `lab` and such that their
indices in `A` are sorted according to `order` (one one of `Forward`,
`Reverse`, or `Unordered`). Iterating over values in order (`Forward` or
`Reverse`) is slower than iterating over unordered values which is the default.

For example:

    @inbounds for val in eachmatch(lab, A, Forward)
        ... # do something
    end

is equivalent to:

    i = findfirst(lab, A)
    @inbounds while i !== nothing
        val = A[i]
        ... # do something
        i = findnext(lab, A, i+1)
    end

"""
Base.eachmatch(lab::L, A::LabelledVector{L}, order::Ordering = Unordered) where {L} =
    LabelledIterator(order, A, lab)

"""
    collect(lab::L, A::LabelledVector{L,V}, order::Ordering = Unordered) -> vals::Vector{V}

yields a vector of the values of `A` labelled by `lab` and such that their
indices in `A` are sorted according to `order` (one one of `Forward`,
`Reverse`, or `Unordered`). Collecting values in order (`Forward` or `Reverse`)
is slower than collectiong unordered values which is the default.

"""
function Base.collect(lab::L, A::LabelledVector{L,V}, order::Ordering = Unordered) where {L,V}
    key = hash(lab)
    start_index = get_first(A, key)
    n = unsafe_count(A, lab, key, start_index)
    B = Array{V}(undef, n)
    if order !== Unordered && n > 1
        I = Array{Int}(undef, n)
        i, j = start_index, 0
        @inbounds while i > 0
            if is_matching(A, i, lab, key)
                I[j += 1] = i
            end
            i = get_next(A, i)
        end
        sort!(I; order=order)
        @inbounds for j ∈ eachindex(B, I)
            B[j] = A[I[j]]
        end
    else
        i, j = start_index, 0
        @inbounds while i > 0
            if is_matching(A, i, lab, key)
                B[j += 1] = A[i]
            end
            i = get_next(A, i)
        end
    end
    return B
end

end # module
