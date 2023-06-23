object_argument(var, type) = Expr(:(::), var, type)
type_argument(type) = Expr(:(::), Expr(:curly, :Type, type))

function parse_kwarg_expr(expr; expr_name::String="", throw_error::Bool=true)
    @match expr begin 
        :($k = $v) => (k, v)
        _ => throw_error ? error("Expected a `key = value` expression" * (!isempty(expr_name) ? " from `$expr_name`" : "") * ", got `$expr") : nothing
    end
end

function is_unionall_type(ex::Expr)
    @switch ex begin 
        @case Expr(:curly, T, args...)
            return true
        @case _ 
            return false 
    end
end
is_unionall_type(ex) = false

function union_all_type_and_param(ex::Expr)
    @switch ex begin 
        @case Expr(:curly, T, args...)
            return T, args
        @case _ 
            error("Expression `$ex` is not of the form `A{T...}`")
    end
end
union_all_type_and_param(ex) = ex

function wrap_type_expr(T; additional_params=Symbol[])
    if is_unionall_type(T)
        _, params = union_all_type_and_param(T)
        all_params = [params..., additional_params...]
        return t->Expr(:where, t, all_params...)
    else
        return identity 
    end
end

function replace_placeholder(x::Symbol, replace_values::Vector{<:Pair{Symbol,<:Any}}) 
    for (old,new) in replace_values
        if x === old 
            return new, true
        end
    end
    return x, false
end
replace_placeholder(x, replace_values) = (x, false)

function replace_placeholder(x::Expr, replace_values::Vector{<:Pair{Symbol,<:Any}})
    replaced = false
    new_expr = Expr(x.head)
    for arg in x.args 
        new_arg, arg_replaced = replace_placeholder(arg, replace_values)
        push!(new_expr.args, new_arg)
        replaced |= arg_replaced
    end
    return new_expr, replaced
end

identity_map_expr(obj_expr, forwarded_expr) = forwarded_expr

function parse_map_func_expr(map_func_expr)
    if ((_, arg_replaced) = replace_placeholder(map_func_expr, [arg_placeholder => arg_placeholder]); arg_replaced)
        return let map_func_expr=map_func_expr
            (obj_expr, t::Expr) ->  replace_placeholder(map_func_expr, [obj_placeholder => obj_expr, arg_placeholder => t])[1]
        end
    else
        return nothing 
    end
end

function parse_map_expr(map_expr)
    kv = parse_kwarg_expr(map_expr; throw_error=false)
    isnothing(kv) && return nothing 
    key, value = kv 
    key != :map && return nothing
    return parse_map_func_expr(value)
end
