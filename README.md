# ForwardMethods

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://curtd.github.io/ForwardMethods.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://curtd.github.io/ForwardMethods.jl/dev/)
[![Build Status](https://github.com/curtd/ForwardMethods.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/curtd/ForwardMethods.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage Status](https://coveralls.io/repos/github/curtd/ForwardMethods.jl/badge.svg)](https://coveralls.io/github/curtd/ForwardMethods.jl)

`ForwardMethods` provides macros that automate some of the boilerplate involved when using composition for object polymorphism. This package is essentially fancy copy + paste for forwarding function definitions, as well as providing automatically generated generic interfaces for struct types. 

# `@forward_methods` 
Given a hypothetical definition for type `T` with a subfield `s` of type `S`, i.e., 

```julia
struct T 
    ...
    s::S
    ...
end
```
suppose that type `S` has a number of existing functions defined as `f₁(..., obj::S, ...; kwargs...)`, ..., `fₙ(..., obj::S, ...; kwargs...)`. In the argument list here, `...` indicates a fixed number of preceding or following arguments (and not a Julia splatting pattern).

If you wanted to define instances of these methods for `x::T` by forwarding them to `getfield(x, :s)`, the normal boilerplate code you would have to write in standard Julia would be 
```julia
    for f in (:f₁, ..., :fₙ)
        @eval $f(..., x::T, ...) = $f(..., getfield(x, :s), ...)
    end
```
which works fine when all of the functions `fᵢ` have same number of arguments and the position of the argument of type `S` is the same in each argument list. If this is not the case, it can be fairly tedious to generate all of the specific signature expressions to evaluate. 

The `@forward_methods` macro automates much of this boilerplate away by only requiring you to specify the type, the fieldname, and the signature of the methods you'd like to forward. 

To take a concrete example, suppose we define 
```julia
    struct T
        s::Vector{Int}
    end
```

We'd like to forward a number of method signatures corresponding to the `AbstractArray` interface. We can write this compactly using `@forward_methods` as:

```julia
    @forward_methods T field=s Base.length(x::T) Base.getindex(x::T, i::Int) Base.setindex!(x::T, v, i::Int)
```

Here the position of the argument of interest will be inferred from the type annotation `::T`. We can write this even more compactly as

```julia 
    @forward_methods T field=s Base.length Base.getindex(_, i::Int) Base.setindex!(x::T, v, i::Int)
```

Here a `0-`argument expression `Base.length` is expanded to `Base.length(x::T)` and the placeholder underscore `_` is expanded to `x::T`.

Rather than defining a forwarded method with an object argument to its child value, e.g., `x::T` -> `getfield(x, :s)` as above, you can also forward the method with a type argument `::Type{T}` to `fieldtype(T, :s)` as follows 

```julia 
    @forward_methods T field=s Base.eltype(::Type{T}) Base.IndexStyle(::Type{T})
```

Parametric types and type signatures are also supported, so if you have, say, 
```julia 
struct A{B}
    d::Dict{String, B}
end
```

you can easily forward parametric / non-parametric method definitions to field `d` simultaneously via  
```julia 
    @forward_methods A{B} field=d (Base.getindex(a::A{B}, k::String) where {B}) Base.keys(x::A) Base.values(_)
```

Letting `x::T` denote the object of interest, the value of the keyword argument `field = _field` has the following effects on the generated expressions:

- If `_field = k` with `k::Symbol` or a `k::QuoteNode`, or an expression of the form `getfield(_, k)`, in which case methods will be forwarded to `getfield(x, k)`

- If `_field = a.b.c. ... .z` is a dotted expression, methods will be forwarded to `getfield(getfield(... getfield(getfield(x, :a), :b), ...), :z)`

- If `_field` is an expression of the form `getproperty(_, k)`, the method instances will be forwarded to `getproperty(x, k)`

- If `_field` is an expression of the form `t[args...]`, the method instances will be forwarded to `x[args...]`

- If `_field` is an expression of the form `f(_)` for a single-argument function `f`, the method instances will be forwarded to `f(x)`


# `@forward_interface` 
If the type of `S` has a known interface (e.g., a fixed set of methods defined with a particular type signature), it may be more convenient to forward the entire suite of methods for that interface to objects `x::T`, rather than to specify each method signature individually.

```julia 
    @forward_interface T field=_field interface=_interface [kwargs...]
```

Here `T` and `_field` are as above and `_interface` is one of a preset number of values, namely `iteration`, `indexing`, `array`, `dict`, `getfields`, and `setfields`. The value of `_interface` determines the specific forwarded method signatures that are generated, e.g., 

```julia
struct B
    b::Dict{String, Int}
end

@forward_interface B field=b interface=dict 

b = B(Dict{String,Int}())
```

`b` can now be used as a drop-in replacement in any method where a `Dict{String,Int}` is supported.

Note: certain methods for certain interfaces (e.g., `Base.similar` for the Array interface) are not included in this macro as direct method forwarding would not make sense to apply in these cases. 

The `getfields` and `setfields` interfaces are dynamically generated based on the fields of type `T`. 

When `interface=getfields`, this macro forwards methods of the form `$field(x::T) = getfield(x, $field)` for each `field ∈ fieldnames(T)`

When `interface=setfields`, this macro forwards methods of the form `$field!(x::T, value) = setfield!(x, $field, value)` for each `field ∈ fieldnames(T)`

# `@define_interface`
Certain interfaces are defined for objects `x::T` that don't involve explicit forwarding to a fixed-field, per-se, but can be generally useful. 

## `properties` interface 
The `properties` interface allows a unified `Base.propertynames`, `Base.getproperty`, and `Base.setproperty!` interface for `x::T` which is composed of subfields `k1, k2, ..., kn` whose fields should also be included in the properties of `x`. This pattern arises when creating composite types. For example,

```julia
julia> struct A
           key1::Int
           key2::Bool
       end

julia> struct B
           key3::String
           key4::Float64
       end

julia> struct C
           a::A
           b::B
       end

julia> @define_interface C interface=properties delegated_fields=(a,b)

julia> c = C(A(1, true), B("a", 0.0))
C(A(1, true), B("a", 0.0))

julia> (key1=c.key1, key2=c.key2, key3=c.key3, key4=c.key4, a=c.a, b=c.b)
(key1 = 1, key2 = true, key3 = "a", key4 = 0.0, a = A(1, true), b = B("a", 0.0))
```

Essentially the fields of `c.a` and `c.b` has been flattened to provide a unified view of the properties of `c`.

## `equality` interface
The `equality` interface defines `Base.==` or `Base.isequal` for objects of `T` in the obvious way, i.e., 

```julia
    Base.:(==)(x::T, y::T) = all( getfield(x,k) == getfield(y,k) for k in fieldnames(T) )
```

There are some configurable options for this macro, so you can do some fancier equality comparisons such as 

```julia
julia> struct E 
           d::Dict{Symbol,Int}
       end

julia> Base.propertynames(e::E) = collect(keys(getfield(e, :d)))

julia> Base.getproperty(e::E, k::Symbol) = getfield(e, :d)[k]

julia> Base.setproperty!(e::E, k::Symbol, v::Int) = getfield(e, :d)[k] = v

julia> @define_interface E interface=equality compare_fields=propertynames

julia> 

julia> e = E(Dict(:a => 1))
E(Dict(:a => 1))

julia> e == E(Dict(:a => 1))
true

julia> e == E(Dict(:a => 2))
false
```


## Similar Packages/Functionality
- [ReusePatterns.jl](https://github.com/gcalderone/ReusePatterns.jl)
- `@forward` in [Lazy.jl](https://github.com/MikeInnes/Lazy.jl)