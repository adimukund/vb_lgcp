using Cairo
using Colors
using ColorSchemes
using CSV
using DataFrames
using Gadfly

include("./vb.jl")

function get_vb_df(base, comp)
    n = size(base)[1]

    ddict = Dict(
        "Real" => base,
        "Computed" => comp
    )

    source = String[]
    x = Int[]
    y = Int[]
    rate = Float64[]

    for s in ["Real", "Computed"]
        for i in 1:n
            for j in 1:n
                push!(source, s)
                push!(x, i)
                push!(y, j)
                push!(rate, ddict[s][n*(i-1)+j])
            end
        end
    end

    return DataFrame(
        :Source => source,
        :X => x,
        :Y => y,
        :Rate => rate
    )
end

function test_rates(csv_name, base_rates, write = false)
    base_rates = transpose(reduce(hcat, base_rates))

    n = size(base_rates)[1]

    base_counts = [[rand(Poisson(base_rates[i, j])) for j in 1:n] for i in 1:n]
    base_counts = transpose(reduce(hcat, base_counts))

    println("Initial rates: ")
    show(stdout, "text/plain", array_to_printable_mat(base_rates))
    println()

    est_rates, _, est_mean, est_std = variational_bayes(base_counts)
    println("Computed rates: ")
    show(stdout, "text/plain", array_to_printable_mat(est_rates))
    println()

    err = sqrt(mean([((base_rates[i]-est_rates[i])/base_rates[i])^2 for i in 1:length(est_rates)]))
    # err = sqrt(mean(([x^2 for x in vec(base_rates) - est_rates]))
    println("Log normalized RMS error: "*string(err))
    println("Estimated gaussian mean: "*string(est_mean))
    println("Estimated gaussian stdev: "*string(est_std))

    print("Exporting dataframe...")
    if write
        CSV.write(csv_name, get_vb_df(base_rates, est_rates))
    end
    println("Done!")

    return est_rates
end

function test_vb_5x5()
    base_rates = [
        [1, 1, 1, 1, 1],
        [1, 10, 50, 10, 1],
        [1, 50, 100, 50, 1],
        [1, 10, 50, 10, 1],
        [1, 1, 1, 1, 1],
    ]
    test_rates("../data/test_vb_5x5.csv", base_rates, true)
end

function test_vb_2hills()
    base_rates = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 50, 50, 50, 0, 0, 0, 0, 0, 0],
        [0, 50, 100, 50, 0, 0, 0, 0, 0, 0],
        [0, 50, 50, 50, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 50, 50, 50, 0],
        [0, 0, 0, 0, 0, 0, 50, 100, 50, 0],
        [0, 0, 0, 0, 0, 0, 50, 50, 50, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    base_rates = [[x+1 for x in arr] for arr in base_rates]
    test_rates("../data/test_vb_2hills.csv", base_rates, true)
end

function test_vb_pacman()
    base_rates = [
        [0,  0,  0,  0,  0,  0,  0,  0, 0],
        [0, 10, 10, 10, 10, 10, 10, 10, 0],
        [0, 10, 90, 90, 90, 90, 90, 10, 0],
        [0, 10, 90, 10, 10, 10, 10, 10, 0],
        [0, 10, 90, 10,  0,  0,  0,  0, 0],
        [0, 10, 90, 10, 10, 10, 10, 10, 0],
        [0, 10, 90, 90, 90, 90, 90, 10, 0],
        [0, 10, 10, 10, 10, 10, 10, 10, 0],
        [0,  0,  0,  0,  0,  0,  0,  0, 0],
    ]
    base_rates = [[x+1 for x in arr] for arr in base_rates]
    test_rates("../data/test_vb_pacman.csv", base_rates, true)
end

function repeat_test_vb_pacman(N = 10)
    base_rates = [
        [0,  0,  0,  0,  0,  0,  0,  0, 0],
        [0, 10, 10, 10, 10, 10, 10, 10, 0],
        [0, 10, 90, 90, 90, 90, 90, 10, 0],
        [0, 10, 90, 10, 10, 10, 10, 10, 0],
        [0, 10, 90, 10,  0,  0,  0,  0, 0],
        [0, 10, 90, 10, 10, 10, 10, 10, 0],
        [0, 10, 90, 90, 90, 90, 90, 10, 0],
        [0, 10, 10, 10, 10, 10, 10, 10, 0],
        [0,  0,  0,  0,  0,  0,  0,  0, 0],
    ]
    base_rates = [[x+1 for x in arr] for arr in base_rates]
    rate_matrices = []
    for i in 1:N
        csvname = "../data/repeat_test_vb_pacman_"*string(i)*".csv"
        push!(rate_matrices, array_to_printable_mat(test_rates(csvname, base_rates)))
    end
    rate_matrix = hcat(sum((rate_matrices)/N)...)

    # println(transpose(hcat(base_rates...)))
    # println(rate_matrix)
    repeat_df = get_vb_df(transpose(hcat(base_rates...)), rate_matrix)
    CSV.write("../data/repeat_test_vb_pacman_mean.csv", repeat_df)
    return rate_matrix
end

function repeat_test_gaussian(μ = 4, σ=1, n = 10, num_iters = 3)
    base_rates = rand(LogNormal(μ, σ), n, n)
    base_rates = [base_rates[i, :] for i in 1:n]
    rate_matrices = []

    for i in 1:num_iters
        csvname = "../data/repeat_test_lgrf_"*string(i)*".csv"
        push!(rate_matrices, array_to_printable_mat(test_rates(csvname, base_rates)))
    end
    rate_matrix = hcat(sum((rate_matrices)/num_iters)...)
    repeat_df = get_vb_df(transpose(hcat(base_rates...)), rate_matrix)
    CSV.write("../data/repeat_test_lgrf_mean.csv", repeat_df)
    return rate_matrix
end