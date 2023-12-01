solve() {
    # take first and last digit of each line and sum the numbers
    cat $1 \
        | sed 's/[^0-9]//g' \
        | sed 's/\(.*\)/\1\1/' \
        | sed 's/^\(.\).*\(.\)$/\1\2/' \
        | paste -s -d+ - \
        | bc
}

# part one
solve input

# part two
cat input \
    | sed 's/one/one1one/g' \
    | sed 's/two/two2two/g' \
    | sed 's/three/three3three/g' \
    | sed 's/four/four4four/g' \
    | sed 's/five/five5five/g' \
    | sed 's/six/six6six/g' \
    | sed 's/seven/seven7seven/g' \
    | sed 's/eight/eight8eight/g' \
    | sed 's/nine/nine9nine/g' \
    | solve -