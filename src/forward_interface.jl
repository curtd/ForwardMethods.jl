function forward_interface_method end 

interface_field_name_required(x) = false 
interface_at_macroexpand_time(x) = true
base_forward_expr(f, args...) = Expr(:call, f, args...)

function forward_interface_args(T)
    t = gensym(arg_placeholder)
    obj_arg = object_argument(t, T)
    type_arg = type_argument(T)
    wrap_expr = wrap_type_expr(T)
    call_expr = (f, args...) -> wrap_expr(base_forward_expr(f, args...))
    return obj_arg, type_arg, call_expr
end

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

Any function names specified in `omit::AbstractVector{Symbol}` will not be defined
"""
function iteration_interface(T; omit::AbstractVector{Symbol}=Symbol[])
    obj_arg, type_arg, call_expr = forward_interface_args(T)
    
    method_signatures = Any[]
    if :iterate ∉ omit 
        push!(method_signatures, call_expr(:(Base.iterate), obj_arg))
        push!(method_signatures, call_expr(:(Base.iterate), obj_arg, :state))
    end
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
    obj_arg, _, call_expr = forward_interface_args(T)
    method_signatures = Any[]
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
    ForwardMethods.array_interface(T; index_style_linear::Bool, omit=Symbol[])

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
    obj_arg, type_arg, call_expr = forward_interface_args(T)

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
            push!(method_signatures, call_expr(:(Base.getindex), obj_arg, :(I::AbstractUnitRange{<:Integer})))
        end
        if :setindex! ∉ omit 
            push!(method_signatures, call_expr(:(Base.setindex!), obj_arg, :v, :(i::Int)))
            push!(method_signatures, call_expr(:(Base.setindex!), obj_arg, :v, :(I::AbstractUnitRange{<:Integer})))
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
    ForwardMethods.vector_interface(T; omit=Symbol[])

Forwards the methods for `x::T` from `ForwardMethods.array_interface(T; index_style_linear=true)`, `ForwardMethods.iteration_interface(T)`, and `ForwardMethods.indexing_interface(T)`

Any function names specified in `omit::AbstractVector{Symbol}` will not be defined.
"""
function vector_interface(T; omit::AbstractVector{Symbol}=Symbol[]) 
    array_signatures = array_interface(T; index_style_linear=true, omit=omit)
    iteration_signatures = iteration_interface(T; omit=union([:iterate, :length, :size], omit))
    indexing_signatures = indexing_interface(T; omit=union([:getindex, :setindex], omit))
    return vcat(array_signatures, iteration_signatures, indexing_signatures)
end

interface_field_name_required(::typeof(vector_interface)) = true

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
    obj_arg, type_arg, call_expr = forward_interface_args(T)
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
    obj_arg, _, call_expr = forward_interface_args(T)
    method_signatures = Any[
        call_expr(:(Base.$f), obj_arg) for f in (:lock, :unlock, :trylock, :islocked) if f ∉ omit
    ]
    if :lock ∉ omit 
        push!(method_signatures, call_expr(:(Base.lock), :f, obj_arg))
    end
    return method_signatures
end

"""
    getfields_interface(T; omit=Symbol[])

Given `x::T`, forwards the method `\$field(x::T)` to `getfield(x, \$field)`, for each `field in fieldnames(T)`
"""
function getfields_interface(T; field::Union{Nothing,Symbol}=nothing, omit::AbstractVector{Symbol}=Symbol[])
    return wrap_define_interface(T, :getfields, Base.remove_linenums!(quote 
        local omit_fields = $(Expr(:tuple, QuoteNode.(omit)...))
        local fields = fieldnames($T)
        local def_fields_expr = Expr(:block)
        local var = gensym("x")
        for field in fields 
            if field ∉ omit_fields
                push!(def_fields_expr.args, :($field($var::$$T) = Base.getfield($var, $(QuoteNode(field)))))
            end
        end
        eval(def_fields_expr)
        nothing
    end))
end

interface_at_macroexpand_time(::typeof(getfields_interface)) = false

"""
    setfields_interface(T; omit=Symbol[])

Given `x::T`, forwards the method `\$field!(x::T, value)` to `setfield!(x, \$field, value)`, for each `field in fieldnames(T)`
"""
function setfields_interface(T; field::Union{Nothing,Symbol}=nothing, omit::AbstractVector{Symbol}=Symbol[])
    return wrap_define_interface(T, :setfields, Base.remove_linenums!(quote 
        local omit_fields = $(Expr(:tuple, QuoteNode.(omit)...))
        local fields = fieldnames($T)
        local def_fields_expr = Expr(:block)
        local var = gensym("x")
        for field in fields 
            if field ∉ omit_fields
                push!(def_fields_expr.args, :($(Symbol(string(field)*"!"))($var::$$T, value) = Base.setfield!($var, $(QuoteNode(field)), value)))
            end
        end
        eval(def_fields_expr)
        nothing
    end))
end

interface_at_macroexpand_time(::typeof(setfields_interface)) = false

const default_forward_interfaces = (:iteration, :indexing, :array, :vector, :dict, :lockable, :getfields, :setfields) 

for f in default_forward_interfaces
    @eval forward_interface_method(::Val{$(QuoteNode(f))}) = $(Symbol(string(f)*"_interface"))
end

if VERSION ≥ v"1.10.0-DEV.609"
    _val_type(::Type{Val{S}}) where {S} = S 
    forward_interfaces_available() = Symbol[_val_type(fieldtype(m.sig, 2)) for m in Base.methods(forward_interface_method)]
else
    @method_def_constant forward_interface_method(::Val{::Symbol}) forward_interfaces_available
end

function forward_interface_expr(T, kwargs::Dict{Symbol,Any}=Dict{Symbol,Any}(); _sourceinfo=nothing)
    interfaces = interface_kwarg!(kwargs)
    omit = omit_kwarg!(kwargs)
    map_val = pop!(kwargs, :map, nothing)
    if !isnothing(map_val) && (map_func = parse_map_func_expr(map_val); !isnothing(map_func))
        nothing
    else
        map_func = identity_map_expr
    end

    field_expr = pop!(kwargs, :field, nothing)
    if !isnothing(field_expr)
        field_funcs = parse_field(Expr(:(=), :field, field_expr))
    else
        field_funcs = nothing
    end

    _output = Expr(:block)

    available_interfaces = forward_interfaces_available()

    for interface in interfaces
        interface in available_interfaces || error("No interface found with name $interface -- must be one of `$available_interfaces`")

        f = forward_interface_method(Val(interface))

        if interface_at_macroexpand_time(f)
            isnothing(field_funcs) && error("Expected `field` from keyword arguments for interface `$interface`")

            if interface_field_name_required(f)
                isnothing(field_funcs.type_func) && error("Only fieldname mode for `field` (= $field) supported for interface (= $interface_value)") 
            end
            signatures = f(T; omit, kwargs...)
            output = Expr(:block)
            for signature in signatures 
                push!(output.args, forward_method_signature(T, field_funcs, map_func, signature; _sourceinfo))
            end
            push!(_output.args, output)
        else 
            push!(_output.args, f(T; omit, kwargs...))
        end
    end
    return _output
end

"""
    @forward_interface T [field=_field] [interface=name] [map=_map] [key=value...]

Forwards the methods defined for `interface` to objects of type `T`

# Arguments 
`name` must be one of $(default_forward_interfaces), with `name` value `f` corresponding to the interface definition function `\$f_interface` (e.g., `array` => `array_interface`).

If `name` is either `getfields` or `setfields`, then the `field` keyword argument is ignored 

Otherwise, `_field` must be one of the following expressions
- `k::Symbol` or `k::QuoteNode`, or an expression of the form `getfield(_, k)`, in which case methods will be forwarded to `getfield(x, k)`
- an expression of the form `getproperty(_, k)`, in which case methods will be forwarded to `getproperty(x, k)`
- an expression of the form `t[args...]`, in which case methods will be forwarded to `x[args...]`
- or an expression of the form `f(_)` for a single-argument function `f`, in which case methods will be forwarded to `f(x)`

# Additional Arguments 
The `key=value` pairs will be forwarded to the corresponding interface definition method. In particular, specifying `omit=func1` or `omit=[func1,func2, ..., funcn]` will omit `func1`, ..., `funcn` from being forwarded by this macro.

See also [`@forward_methods`](@ref)
"""
macro forward_interface(T, args...)
    kwargs = Dict{Symbol,Any}()
    for arg in args 
        key, value = parse_kwarg_expr(arg)
        kwargs[key] = value
    end
    return forward_interface_expr(T, kwargs; _sourceinfo=__source__) |> esc
end