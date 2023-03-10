module TestingLabelledData

using LabelledData
#using Compat
using Test

# These versions of findfirst and findall are not available in all Julia
# versions. They are not meant to be fast, just provide replacements.
_findfirst(c::Char, s::AbstractString) = findfirst(x -> x == c, collect(s))
_findall(c::Char, s::AbstractString) = findall(x -> x == c, collect(s))

@testset "LabelledData" begin
    @testset "Labelled vectors with given labels" begin
        # Write your tests here.
        text = "abbacdaagbhabc"
        labs = ["$c" for c in text]
        vals = collect(1:length(labs))
        A = LabelledVector{AbstractString}(labs, vals)
        @test IndexStyle(A) === IndexLinear()
        @test eltype(A) == eltype(vals)
        @test length(A) == length(vals)
        @test size(A) == (length(A),)
        @test axes(A) == map(Base.OneTo, size(A))
        @test firstindex(A) === one(Int)
        @test lastindex(A) === length(A)
        @test A == vals # check that values are the same
        @test count("a", A) == length(_findall('a', text))
        @test count("b", A) == length(_findall('b', text))
        @test count("c", A) == length(_findall('c', text))
        @test count("d", A) == length(_findall('d', text))
        @test count("@", A) == length(_findall('@', text))
        @test length(eachmatch("a", A)) == length(_findall('a', text))
        @test length(eachmatch("b", A)) == length(_findall('b', text))
        @test length(eachmatch("c", A)) == length(_findall('c', text))
        @test length(eachmatch("d", A)) == length(_findall('d', text))
        @test length(eachmatch("@", A)) == length(_findall('@', text))
        @test get(A, "a", nothing) == _findfirst('a', text)
        @test get(A, "b", nothing) == _findfirst('b', text)
        @test get(A, "c", nothing) == _findfirst('c', text)
        @test get(A, "d", nothing) == _findfirst('d', text)
        @test get(A, "@", nothing) == _findfirst('@', text)
        @test A["a"] === get(A, "a", nothing)
        @test A["b"] === get(A, "b", nothing)
        @test A["c"] === get(A, "c", nothing)
        @test A["d"] === get(A, "d", nothing)
        @test_throws KeyError A["@"]
        @test get(A, SubString("Julia", 5:5), nothing) == get(A, "a", nothing)
        @test collect("a", A, Forward) == _findall('a', text)
        @test collect("b", A, Forward) == _findall('b', text)
        @test collect("c", A, Forward) == _findall('c', text)
        @test collect("d", A, Forward) == _findall('d', text)
        @test collect("@", A, Forward) == _findall('@', text)
        @test collect("a", A, Reverse) == reverse(_findall('a', text))
        @test collect("b", A, Reverse) == reverse(_findall('b', text))
        @test collect("c", A, Reverse) == reverse(_findall('c', text))
        @test collect("d", A, Reverse) == reverse(_findall('d', text))
        @test collect("@", A, Reverse) == reverse(_findall('@', text))
        @test sort(collect("a", A)) == _findall('a', text)
        @test sort(collect("b", A)) == _findall('b', text)
        @test sort(collect("c", A)) == _findall('c', text)
        @test sort(collect("d", A)) == _findall('d', text)
        @test sort(collect("@", A)) == _findall('@', text)
        # The following tests work because the value is also the index.
        @test collect(eachmatch("a", A, Forward)) == vals[collect("a", A, Forward)]
        @test collect(eachmatch("b", A, Forward)) == vals[collect("b", A, Forward)]
        @test collect(eachmatch("c", A, Forward)) == vals[collect("c", A, Forward)]
        @test collect(eachmatch("d", A, Forward)) == vals[collect("d", A, Forward)]
        @test collect(eachmatch("@", A, Forward)) == vals[collect("@", A, Forward)]
        @test collect(eachmatch("a", A, Reverse)) == vals[collect("a", A, Reverse)]
        @test collect(eachmatch("b", A, Reverse)) == vals[collect("b", A, Reverse)]
        @test collect(eachmatch("c", A, Reverse)) == vals[collect("c", A, Reverse)]
        @test collect(eachmatch("d", A, Reverse)) == vals[collect("d", A, Reverse)]
        @test collect(eachmatch("@", A, Reverse)) == vals[collect("@", A, Reverse)]
        @test collect(reverse(eachmatch("a", A, Forward))) == vals[collect("a", A, Reverse)]
        @test collect(reverse(eachmatch("b", A, Forward))) == vals[collect("b", A, Reverse)]
        @test collect(reverse(eachmatch("c", A, Forward))) == vals[collect("c", A, Reverse)]
        @test collect(reverse(eachmatch("d", A, Forward))) == vals[collect("d", A, Reverse)]
        @test collect(reverse(eachmatch("@", A, Forward))) == vals[collect("@", A, Reverse)]
        @test collect(reverse(eachmatch("a", A, Reverse))) == vals[collect("a", A, Forward)]
        @test collect(reverse(eachmatch("b", A, Reverse))) == vals[collect("b", A, Forward)]
        @test collect(reverse(eachmatch("c", A, Reverse))) == vals[collect("c", A, Forward)]
        @test collect(reverse(eachmatch("d", A, Reverse))) == vals[collect("d", A, Forward)]
        @test collect(reverse(eachmatch("@", A, Reverse))) == vals[collect("@", A, Forward)]
    end
    @testset "Labelled vectors with computed labels" begin
        # Write your tests here.
        text = "abbacdaagbhabc"
        labs = ["$c" for c in text]
        vals = [(lab,val) for (lab,val) in zip(labs, 1:length(labs))]
        A = LabelledVector{AbstractString}(first, vals)
        @test IndexStyle(A) === IndexLinear()
        @test eltype(A) == eltype(vals)
        @test length(A) == length(vals)
        @test size(A) == (length(A),)
        @test axes(A) == map(Base.OneTo, size(A))
        @test firstindex(A) === one(Int)
        @test lastindex(A) === length(A)
        @test A == vals # check that values are the same
        @test count("a", A) == length(_findall('a', text))
        @test count("b", A) == length(_findall('b', text))
        @test count("c", A) == length(_findall('c', text))
        @test count("d", A) == length(_findall('d', text))
        @test count("@", A) == length(_findall('@', text))
        @test length(eachmatch("a", A)) == length(_findall('a', text))
        @test length(eachmatch("b", A)) == length(_findall('b', text))
        @test length(eachmatch("c", A)) == length(_findall('c', text))
        @test length(eachmatch("d", A)) == length(_findall('d', text))
        @test length(eachmatch("@", A)) == length(_findall('@', text))
        @test A["a"] === get(A, "a", nothing)
        @test A["b"] === get(A, "b", nothing)
        @test A["c"] === get(A, "c", nothing)
        @test A["d"] === get(A, "d", nothing)
        @test_throws KeyError A["@"]
        @test sort(collect("a", A)) == map(i -> (SubString(text,i:i), i), _findall('a', text))
        @test sort(collect("b", A)) == map(i -> (SubString(text,i:i), i), _findall('b', text))
        @test sort(collect("c", A)) == map(i -> (SubString(text,i:i), i), _findall('c', text))
        @test sort(collect("d", A)) == map(i -> (SubString(text,i:i), i), _findall('d', text))
        @test sort(collect("@", A)) == map(i -> (SubString(text,i:i), i), _findall('@', text))
    end
end
end # module
