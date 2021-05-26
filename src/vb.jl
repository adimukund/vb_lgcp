# variational bayes for lgcps
# author: axm

# input: n x n array of emission counts
# output: n x n array of estimated rates


using Distances
using Distributions
using LaTeXStrings
using LinearAlgebra
using Optim
using ProgressBars
using ProgressMeter
using Random
using ReverseDiff

################################################################################
# 
# SOME QUICK NOTES 
# we'll initialize:
#    μ_μ     ⟵ sum(counts)/(num cells)
#    (σ_u)^2 ⟵ variance(# counts per cell)
#    α, β    ⟵ 1.0
# then, we can compute
#    μ ~ N(μ_μ, (σ_μ)^2)
#    σ^2 ~ InvGamma(α, β)
#    then q(μ) ~ Normal, q(σ^2) ~ InvGamma
# 
################################################################################


################################################################################
#    CONSTANTS
################################################################################
δ = 1.312
A = 1
maximum_mean_rate = 1e5

# returns the centroid for the i-th grid point on an n x n grid
function get_centroid(i, n)
    x = Int(round((i-1) ÷ n)) + 0.5
    y = (i-1) % n  + 0.5
    return [x, y]    
end

function H(y_i, A, E_σ_2, C_ii)
    return -A*exp(y_i) - E_σ_2 * C_ii
end

function get_cov_mat(ρ, n)
    C = zeros(n^2, n^2)
    for i in 1:n^2
        for j in 1:n^2
            centroid_i = get_centroid(i, n)
            centroid_j = get_centroid(j, n)
            C[i, j] = exp(-ρ * euclidean(centroid_i, centroid_j)^δ)
        end
    end
    return C
end

function fit_ρ(nk)
    # fit ρ by computing a nonparametric estimated covariance function
    # minimize sum of distances? wouldn't this just give you ρ = 0? 
    # non-parametric form of covariance s.t. 

    # first, compute non-parametric covariance matrix
    # this is an n^2 x n^2 matrix 
    n = size(nk)[1]
    N = zeros(n^2, n^2)
    ex = sum(nk)
    exsq = sum([x^2 for x in nk])
    for i in 1:n^2
        for j in 1:n^2
            N[i, j] = ((nk[i] - ex)*(nk[j]-ex))/((nk[i]^2 - exsq) * (nk[j]^2 - exsq))
        end
    end

    # now, fit ρ to this -- we have assumed δ = 1.312 in the constants section
    function cov_dist(ρ)
        C = get_cov_mat(ρ, n)
        return sqrt(sum([x^2 for x in N - C]))
    end

    ρ = optimize(cov_dist, 0, 100).minimizer
    return ρ
end

function initialize_vectors(nk)
    n = Int(sqrt(length(nk)))

    μ_μ = log(sum(nk)/n)
    σ_μ = log(std(nk))
    α   = 1
    β   = 1

    E_σ_2 = (α + n^2/2)/(β)

    ρ = fit_ρ(nk)
    C = get_cov_mat(ρ, n)
    C_inv = inv(C)

    μ_y_q = rand(Normal(μ_μ, σ_μ), n^2)
    Σ_y_q = Diagonal([-H(μ_y_q[i], A, E_σ_2, C[i, i])^-1 for i in 1:n^2])

    return [
        n, μ_μ, σ_μ, α, β,
        E_σ_2, ρ, C, C_inv, 
        μ_y_q, Σ_y_q
    ]
end

function update_mean_y_q(μ_y_q, A, E_σ_2, nk, μ_μ_q, n, C_inv) 
    new_vec = zeros(n^2)
    errors = zeros(n^2)

    for i in 1:n^2
        function get_entry_diff(uqyi)
            sum_delta = 0
            delta_vec = μ_y_q - μ_μ_q*ones(n^2)
            for j in 1:n^2
                if (j != i)
                    sum_delta += C_inv[i, j] * delta_vec[j]
                end
            end

            u  = -A * exp(uqyi) - E_σ_2*C_inv[i, i]*uqyi + nk[i]
            u += μ_μ_q*E_σ_2*C_inv[i,i] - E_σ_2 *sum_delta

            return u^2 # directly minimize this
        end
        optim = optimize(
            x -> get_entry_diff(first(x)), 
            [μ_y_q[i]],
            NewtonTrustRegion();
            # BFGS();
            autodiff = :forward
            )
        # print(optim)
        new_vec[i] = optim.minimizer[1]
        errors[i] = optim.minimum
    end
    return new_vec
end

function array_to_printable_mat(arr)
    arr_size = Int(sqrt(length(arr)))
    mat = [[round(arr[i-1 + j]; digits=3) for j in range(1, stop=arr_size)] 
                                          for i in range(1, stop=length(arr), step=arr_size)]
    return mat
end

function variational_bayes(nk, debug = false)

    function printdb(s)
        if debug
            print(string(s) * " ")
        end
    end

    function printlndb(s)
        if debug
            println(string(s) * " ")
        end
    end

    printdb("Initializing vectors...")
    # steps 1-3
    n, μ_μ, σ_μ, α, β, E_σ_2, _, C, C_inv, μ_y_q, Σ_y_q = initialize_vectors(nk)
    μ_μ_q = deepcopy(μ_μ)
    σ_μ_q = deepcopy(σ_μ)
    one = ones(n^2)
    oneT = transpose(one)
    Δ = 1e3
    ϵ = 1e-100 # smallish number
    F_old = -1e-8 # very small number
    printlndb("done.")

    printlndb("Computing updates...")
    # printlndb("Looking to converge with threshold ϵ="*string(ϵ))

    prog = ProgressThresh(ϵ, "Current Δ: ")

    while Δ > ϵ
        # step 4
        μ_y_q = update_mean_y_q(μ_y_q, A, E_σ_2, nk, μ_μ_q, n, C_inv)
        H_q = vec(map(i -> -1/H(μ_y_q[i], A, E_σ_2, C_inv[i, i]), 1:n^2))
        Σ_y_q = Diagonal(H_q)

        # step 5
        μ_μ_q_num = E_σ_2*transpose(μ_y_q)*C_inv*one + μ_μ/σ_μ^2
        μ_μ_q_den = E_σ_2*oneT*C_inv*one + 1/σ_μ^2
        μ_μ_q = μ_μ_q_num/μ_μ_q_den
        σ_μ_q = sqrt((E_σ_2*oneT*C_inv*one + 1/σ_μ^2)^-1)

        # step 6
        μ_offset = μ_y_q - ones(n^2)*μ_μ_q
        β_q = β   + 0.5*(transpose(μ_offset)*C_inv*μ_offset) 
        β_q = β_q + σ_μ_q^2*oneT*C_inv*one 
        E_σ_2 = (α + n^2/2)/β_q

        # step 7
        F_q = 0
        for i in 1:n^2
            F_q += μ_y_q[i]*nk[i] - A*exp(μ_y_q[i] - 0.5/H(μ_y_q[i], A, E_σ_2, C_inv[i,i]))
        end
        F_q -= 0.5 * E_σ_2 * log(det(C)) # really not sure what the E is here
        F_q -= 0.5*((μ_μ_q - μ_μ)^2 + σ_μ_q)^2/σ_μ^2
        for i in 1:n^2
            F_q += 0.5*log(-1/H(μ_y_q[i], A, E_σ_2, C_inv[i, i]))
        end
        F_q += 0.5*log(σ_μ_q^2)

        Δ = abs(F_q/F_old - 1)

        if isnan(Δ)
            println("Δ=NaN, returning nothing!")
            return nothing
        end

        # Δ = F_q/F_old - 1
        F_old = F_q
        ProgressMeter.update!(prog, Δ)
        sleep(0.01)
    end

    rates = vec([exp(x) for x in μ_y_q])
    printlndb("Converged with Δ="*string(Δ))

    if debug
        printlndb("Actual rates: ")
        show(stdout, "text/plain", array_to_printable_mat(base_rates))
        println()

        printlndb("Computed rates: ")
        show(stdout, "text/plain", array_to_printable_mat(rates))
        println()
    end
    printlndb("Done!")

    return rates, Σ_y_q, μ_μ_q, σ_μ_q
end