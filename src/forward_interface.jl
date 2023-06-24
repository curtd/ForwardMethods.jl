interface_method(x) = nothing
interface_field_name_required(x) = false 

base_forward_expr(f, args...) = Expr(:call, f, args...)

# Iteration, indexing, array interface methods from https://docs.julialang.org/en/v1/manual/interfaces/#Interfaces
"""
    ForwardMethods.iteration_interface(T; omit=Symbol[])

Forwards the following methods for `x::T`:
- `Base.iterate(x::T)`
- `Base.iterate(x::T, state)`
- `Base.IteratorSize(::Type{T})`
- `Base.IteratorEltype(::Type{T})`
- `Base.eltype(::Type{T})`
- `Base.length(x::T)`
- `Base.size(x::T)`
- `Base.isdone(x::T)`
- `Base.isdone(x::T, state)`

Any function names specified in `omit::AbstractVector{Symbol}` (aside from `:iterate`) will not be defined
"""
function iteration_interface(T; omit::AbstractVector{Symbol}=Symbol[])
    t = gensym(arg_placeholder)
    obj_arg = object_argument(t, T)
    type_arg = type_argument(T)
    wrap_expr = wrap_type_expr(T)
    call_expr = (f, args...) -> wrap_expr(base_forward_expr(f, args...))
  
    method_signatures = Any[
        call_expr(:(Base.iterate), obj_arg),
        call_expr(:(Base.iterate), obj_arg, :state)
    ]
    for f in (:IteratorSize, :IteratorEltype, :eltype)
        f ∈ omit && continue 
        push!(method_signatures, call_expr(:(Base.$f), type_arg))
    end
    for f in (:length, :size, :isdone)
        f ∈ omit && continue
        push!(method_signatures, call_expr(:(Base.$f), obj_arg) )
    end
    if :isdone ∉ omit
        push!(method_signatures, call_expr(:(Base.isdone), obj_arg, :state) )
    end
    return method_signatures
end

interface_field_name_required(::typeof(iteration_interface)) = true

"""
    ForwardMethods.indexing_interface(T; omit=Symbol[])

Forwards the following methods for `x::T`:
- `Base.getindex(x::T, index)`
- `Base.setindex!(x::T, value, index)`
- `Base.firstindex(x::T)`
- `Base.lastindex(x::T)`

Any function names specified in `omit::AbstractVector{Symbol}` will not be defined
"""
function indexing_interface(T; omit::AbstractVector{Symbol}=Symbol[])
    t = gensym(arg_placeholder)
    obj_arg = object_argument(t, T)
    method_signatures = Any[]
    wrap_expr = wrap_type_expr(T)
    call_expr = (f, args...) -> wrap_expr(base_forward_expr(f, args...))
    if :getindex ∉ omit 
        push!(method_signatures, call_expr(:(Base.getindex), obj_arg, :i))
    end
    if :setindex! ∉ omit 
        push!(method_signatures, call_expr(:(Base.setindex!), obj_arg, :v, :i))
    end
    if :firstindex ∉ omit 
        push!(method_signatures, call_expr(:(Base.firstindex), obj_arg))
    end
    if :lastindex ∉ omit 
        push!(method_signatures, call_expr(:(Base.lastindex), obj_arg))
    end
    return method_signatures
end

# Note: not a straightforward way to forward Base.similar for generic types, so it is not included here
"""
    ForwardMethods.array(T; index_style_linear::Bool, omit=Symbol[])

Forwards the following methods for `x::T`:
- `Base.size(x::T)`
- `Base.iterate(x::T)`
- `Base.iterate(x::T, state)`
- `Base.length(x::T)`
- `Base.IndexStyle(::Type{T})`
- `Base.getindex(x::T, index)`
- `Base.setindex!(x::T, value, index)`

The signatures for `Base.getindex` + `Base.setindex!` will be set 
according to the value of `index_style_linear` 

Any function names specified in `omit::AbstractVector{Symbol}` will not be defined
"""
function array_interface(T; index_style_linear::Bool, omit::AbstractVector{Symbol}=Symbol[])
    t = gensym(arg_placeholder)
    obj_arg = object_argument(t, T)
    type_arg = type_argument(T)
    wrap_expr = wrap_type_expr(T)
    call_expr = (f, args...) -> wrap_expr(base_forward_expr(f, args...))

    method_signatures = Any[
        call_expr(:(Base.$f), obj_arg) for f in (:size, :iterate, :length) if f ∉ omit
    ]
    if :iterate ∉ omit 
        push!(method_signatures, call_expr(:(Base.iterate), obj_arg, :state))
    end
    if :IndexStyle ∉ omit
        push!(method_signatures, call_expr(:(Base.IndexStyle), type_arg))
    end
    if index_style_linear
        if :getindex ∉ omit
            push!(method_signatures, call_expr(:(Base.getindex), obj_arg, :(i::Int)))
        end
        if :setindex! ∉ omit 
            push!(method_signatures, call_expr(:(Base.setindex!), obj_arg, :v, :(i::Int)))
        end
    else
        if :getindex ∉ omit
            push!(method_signatures, call_expr(:(Base.getindex), obj_arg, :(I...)))
        end
        if :setindex! ∉ omit
            push!(method_signatures, call_expr(:(Base.setindex!), obj_arg, :v, :(I...)))
        end
    end
    return method_signatures
end

interface_field_name_required(::typeof(array_interface)) = true

"""
    ForwardMethods.dict_interface(T; omit=Symbol[])

Forwards the following methods for `x::T`:
- `Base.keys(x::T)`
- `Base.values(x::T)`
- `Base.pairs(x::T)`
- `Base.length(x::T)`
- `Base.isempty(x::T)`
- `Base.empty!(x::T)`
- `Base.iterate(x::T)`
- `Base.iterate(x::T, state)`
- `Base.pop!(x::T, key)`
- `Base.delete!(x::T, key)`
- `Base.haskey(x::T, key)`
- `Base.getindex(x::T, key)`
- `Base.setindex!(x::T, value, key)`
- `Base.get(x::T, key, default_value)`
- `Base.get!(default::Callable, x::T, key)`
- `Base.in(value, x::T)`

Any function names specified in `omit::AbstractVector{Symbol}` will not be defined
"""
function dict_interface(T; omit::AbstractVector{Symbol}=Symbol[])
    t = gensym(arg_placeholder)
    obj_arg = object_argument(t, T)
    type_arg = type_argument(T)
    wrap_expr = wrap_type_expr(T)
    call_expr = (f, args...) -> wrap_expr(base_forward_expr(f, args...))
    method_signatures = Any[
        call_expr(:(Base.$f), obj_arg) for f in (:keys, :values, :pairs, :length, :isempty, :empty!, :iterate) if f ∉ omit
    ]
    for f in (:eltype, :keytype, :valtype)
        f in omit && continue
        push!(method_signatures, call_expr(:(Base.$f), type_arg))
    end
    if :iterate ∉ omit
        push!(method_signatures, call_expr(:(Base.iterate), obj_arg, :state))
    end
    for f in (:pop!, :getindex, :haskey, :delete!)
        f in omit && continue
        push!(method_signatures, call_expr(:(Base.$f), obj_arg, :key))
    end
    if :pop! ∉ omit 
        push!(method_signatures, call_expr(:(Base.pop!), obj_arg, :key, :default))
    end
    if :get ∉ omit
        push!(method_signatures, call_expr(:(Base.get), obj_arg, :key, :default))
    end
    if :get! ∉ omit
        push!(method_signatures, call_expr(:(Base.get!), :(default::Base.Callable), obj_arg, :key))
    end
    if :setindex! ∉ omit
        push!(method_signatures, call_expr(:(Base.setindex!), obj_arg, :value, :key))
    end
    if :in ∉ omit
        push!(method_signatures, call_expr(:(Base.in), :value, obj_arg))
    end
    return method_signatures
end

interface_field_name_required(::typeof(dict_interface)) = true

"""
    ForwardMethods.lockable_interface(T; omit=Symbol[])

Forwards the following methods for `x::T`:
- `Base.lock(x::T)`
- `Base.lock(f, x::T)`
- `Base.unlock(x::T)`
- `Base.trylock(x::T)`
- `Base.islocked(x::T)`

Any function names specified in `omit::AbstractVector{Symbol}` will not be defined
"""
function lockable_interface(T; omit::AbstractVector{Symbol}=Symbol[])
    t = gensym(arg_placeholder)
    obj_arg = object_argument(t, T)
    wrap_expr = wrap_type_expr(T)
    call_expr = (f, args...) -> wrap_expr(base_forward_expr(f, args...))
    method_signatures = Any[
        call_expr(:(Base.$f), obj_arg) for f in (:lock, :unlock, :trylock, :islocked) if f ∉ omit
    ]
    if :lock ∉ omit 
        push!(method_signatures, call_expr(:(Base.lock), :f, obj_arg))
    end
    return method_signatures
end

const interfaces_defined = (:iteration, :indexing, :array, :dict, :lockable)

for f in interfaces_defined 
    @eval interface_method(::Type{Val{$(QuoteNode(f))}}) = $(Symbol(string(f)*"_interface"))
end

function forward_interface_expr(T, field_expr, interface_expr, kwargs::Dict{Symbol,Any}=Dict{Symbol,Any}(); _sourceinfo=nothing)
    interface_kwarg = parse_kwarg_expr(interface_expr; expr_name="interface", throw_error=false)
    if isnothing(interface_kwarg)
        interface_value = interface_kwarg
    else
        interface_key, interface_value = interface_kwarg 
        interface_key == :interface || error("Expected `interface` key from $interface_expr expression, got $interface_key")
    end
    interface_value isa Symbol || error("interface value (= $interface_value) must be a Symbol, got typeof(interface_value) = $(typeof(interface_value))")
    map_val = pop!(kwargs, :map, nothing)
    if !isnothing(map_val) && (map_func = parse_map_func_expr(map_val); !isnothing(map_func))
        nothing
    else
        map_func = identity_map_expr
    end

    omit_val = pop!(kwargs, :omit, nothing)
    if !isnothing(omit_val)
        omit = @switch omit_val begin 
            @case ::Symbol 
                Symbol[omit_val]
            @case Expr(:vect, args...) && if all(arg isa Symbol for arg in args) end 
                convert(Vector{Symbol}, collect(args))
            @case _ 
                error("omit value (=$omit_val) must be a `Symbol` or a `vect` expression with `Symbol` arguments")
        end
    else 
        omit = Symbol[]
    end

    f = interface_method(Val{interface_value})
    isnothing(f) && error("No interface found with name $interface_value -- must be one of `$interfaces_defined`")
    field_funcs = parse_field(field_expr)
    if interface_field_name_required(f)
        isnothing(field_funcs.type_func) && error("Only fieldname mode for `field` (= $field) supported for interface (= $interface_value)") 
    end
    signatures = f(T; omit, kwargs...)
    output = Expr(:block)
    for signature in signatures 
        push!(output.args, forward_method_signature(T, field_funcs, map_func, signature; _sourceinfo))
    end
    return output
end

"""
    @forward_interface T [field=_field] [interface=_interface] [map=_map] [kwargs...]

Forwards the methods defined for `interface` to objects of type `T`

`_field` must be one of the following expressions
- `k::Symbol` or `k::QuoteNode`, or an expression of the form `getfield(_, k)`, in which case methods will be forwarded to `getfield(x, k)`
- an expression of the form `getproperty(_, k)`, in which case methods will be forwarded to `getproperty(x, k)`
- an expression of the form `t[args...]`, in which case methods will be forwarded to `x[args...]`
- or an expression of the form `f(_)` for a single-argument function `f`, in which case methods will be forwarded to `f(x)`

`_interface` must be one of $(interfaces_defined), with `_interface` value `f` corresponding to the interface definition function `\$f_interface` (e.g., `array` => `array_interface`).

The `key=value` pairs will be forwarded to the corresponding interface definition method. In particular, specifying `omit=func1` or `omit=[func1,func2, ..., funcn]` will omit `func1`, ..., `funcn` from being forwarded by this macro.

See also [`@forward_methods`](@ref)
"""
macro forward_interface(T, field, interface, args...)
    kwargs = Dict{Symbol,Any}()
    for arg in args 
        key, value = parse_kwarg_expr(arg)
        kwargs[key] = value
    end
    return forward_interface_expr(T, field, interface, kwargs; _sourceinfo=__source__) |> esc
end