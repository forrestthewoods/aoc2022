main :: () {
    // day01();
    // day02();
    // day03();
    day04();
}

// --------------------------------------------------------
// Utilities
// --------------------------------------------------------
next_line :: (s : *string) -> string {
    assert(s.count > 0);
    
    result : string;

    // line ending (windows)
    eol : string = "\r\n";

    // find next line ending
    end := find_index_from_left(<<s, eol);
    
    // end of string
    if end == -1 {
        end = s.count;
    }

    // compute result
    result = <<s;
    result.count = end;

    // advance string
    increment := end + eol.count;
    s.data += increment;
    s.count -= increment;
    
    return result;
}

map :: (array: [] $T, f: (T) -> $R) -> [] R {
    result: [..] R;

    // This map is hardcoded to return more temporary storage!
    // We could specify this with an optional parameter if we wanted to.
    result.allocator = temp;
    
    for array array_add(*result, f(it));

    return result;
}


// --------------------------------------------------------
// Day 01
// --------------------------------------------------------
day01 :: () {
    day01_test();
    day01_solve();
}

day01_solve :: () {
    file, success := read_entire_file("data/day01.txt");
    assert(success);

    elves := day01_parse(file);

    print("day01 part1: %\n", day01_part1(elves));
    print("day01 part2: %\n", day01_part2(elves));
}

day01_test :: () {
    file, success := read_entire_file("data/day01_example.txt");
    assert(success);

    elves := day01_parse(file);
    assert(day01_part1(elves) == 24000);
    assert(day01_part2(elves) == 45000);
}

day01_parse :: (file: string) -> [..][..]u32 {
    elves : [..][..]u32;

    // create and store new elf
    cur_elf : *[..]u32;

    // Iterate file line by line
    start := 0;

    while start < file.count {
        // Find end of line
        end := find_index_from_left(file, #char "\r", start);
        
        // empty line
        if start == end {
            // Clear elf
            cur_elf = null;

            // Advance past \r\n
            start += 2;
            continue;
        }

        // end of file
        if end == -1 {
            break;
        }

        if cur_elf == null {
            // Create new elf
            new_elf : [..]u32;
            array_add(*elves, new_elf);

            // Update cur_elf pointer
            cur_elf = *elves[elves.count - 1];
        }

        // Create string for line
        line : string;
        line.data = file.data + start;
        line.count = end - start;

        // Parse line
        success, parsed := scan("%i", line);
        if success {
            weight : u32 = << cast(*u32)parsed[0].value_pointer;
            array_add(cur_elf, weight);
        }

        // Advance start past \r\n
        start = end + 2;
    }

    return elves;
}

day01_part1 :: (elves: [..][..]u32) -> s64 {
    max_weight := 0;

    // iterate elves
    for elf : elves {
        // sum elf weight
        elf_weight := 0;
        for weight : elf {
            elf_weight += weight;
        }

        // keep largest
        max_weight = max(elf_weight, max_weight);
    }

    return max_weight;
}

day01_part2 :: (elves: [..][..]u32) -> s64 {
    sums : [..]s64;

    // iterate elves
    for elf : elves {
        // sum elf weight
        elf_weight := 0;
        for weight : elf {
            elf_weight += weight;
        }

        // store elf weight
        array_add(*sums, elf_weight);
    }

    // sort
    compare :: (a: s64, b: s64) -> s64 {
        delta := a - b;
        if delta < 0 return  1;
        if delta > 0 return -1;
        return 0;
    }

    sorted_sums := quick_sort(sums, compare);

    result := sorted_sums[0] + sorted_sums[1] + sorted_sums[2];
    return result;
}

// --------------------------------------------------------
// Day 02
// --------------------------------------------------------
day02 :: () {
    day02_test();
    day02_solve();
}

day02_solve :: () {
    input, success := read_entire_file("data/day02.txt");
    assert(success);

    moves := day02_parse(input);
    print("day02 part1: %\n", day02_part1(moves));
    print("day02 part2: %\n", day02_part2(moves));
}

day02_test :: () {
    input, success := read_entire_file("data/day02_example.txt");
    assert(success);

    moves := day02_parse(input);
    
    assert(day02_part1(moves) == 15);
    //assert(day02_part2(moves) == 12);
}

day02_parse :: (input: string) ->  [..]d2_round {
    result : [..]d2_round;

    while input.count > 0 {
        line := next_line(*input);
        assert(line.count == 3);

        entry: d2_round;
        entry.a = line[0] - #char "A";
        entry.b = line[2] - #char "X";

        array_add(*result, entry);
    }

    return result;
}

day02_part1 :: (rounds : [..]d2_round) -> s64 {
    result : s64 = 0;

    for round : rounds {
        result += score_round(round);
    }

    return result;
}

day02_part2 :: (rounds : [..]d2_round) -> s64 {
    result : s64 = 0;

    // constants
    rock : u8 = 0;
    paper : u8 = 1;
    scissors : u8 = 2;

    for round : rounds {
        
        // modify round
        new_round := round;

        if round.b == 0 {
            // lose
            map : [3]u8; 
            map[0] = scissors;
            map[1] = rock;
            map[2] = paper;

            new_round.b = map[round.a];
        } else if round.b == 1 {
            // tie
            new_round.b = round.a;
        } else {
            // win
            map : [3]u8; 
            map[0] = paper;
            map[1] = scissors;
            map[2] = rock;

            new_round.b = map[round.a];
        }

        result += score_round(new_round);
    }

    return result;
}

score_round :: (r: d2_round) -> s64 {
    result : s64 = 0;

    // constants
    rock : u8 = 0;
    paper : u8 = 1;
    scissors : u8 = 2;

    // get moves
    a := r.a;
    b := r.b;
    
    // Points for player move
    // rock = 1, paper = 2, scissors = 3
    result += b - rock + 1; 

    // 3 points for tie
    if a == b {
        result += 3;
    }

    // 6 points for win
    if (b == paper && a == rock) 
        || (b == scissors && a == paper) 
        || (b == rock && a == scissors) 
    {
        result += 6;
    }

    return result;
}

d2_round :: struct {
    a: u8;
    b: u8;
};

// --------------------------------------------------------
// Day 03
// --------------------------------------------------------
day03 :: () {
    day03_test();
    day03_solve();
}

day03_test :: () {
    input, success := read_entire_file("data/day03_example.txt");
    assert(success);

    assert(day03_part1(input) == 157);
    assert(day03_part2(input) == 70);
}

day03_solve :: () {
    input, success := read_entire_file("data/day03.txt");
    assert(success);

    print("day03 part1: %\n", day03_part1(input));
    print("day03 part2: %\n", day03_part2(input));
}

day03_part1 :: (input : string) -> u64 {
    result : u64;

    while input.count > 0 {
        line := next_line(*input);

        // split line
        len := line.count;
        assert(len % 2 == 0);
        mid := len / 2;

        left := slice(line, 0, mid);
        right := slice(line, mid, len - mid);

        // find duplicate
        dupe : u8 = 0;
        
        idx := 0;
        while idx < left.count {
            defer idx += 1;

            char := left[idx];
            if find_index_from_left(right, char) != -1 {
                dupe = char;
                break;
            }
        }
        assert(dupe != 0);

        // assume dupe was found
        if dupe <= #char "Z" {
            value := dupe - #char "A" + 27;
            result += dupe - #char "A" + 27;
        } else {
            value := dupe - #char "a" + 1;
            result += dupe - #char "a" + 1;
        }
    }

    return result;
}

day03_part2 :: (input : string) -> u64 {
    result : u64;

    // get 3 lines
    while input.count > 0 {
       a := next_line(*input);
       b := next_line(*input);
       c := next_line(*input);

        // helper to calculate bitmask for a single line
        calculate_mask :: (line: string) -> u64 {
            mask: u64 = 0;

            for 0..line.count-1 {
                // convert to mask-index
                c := line[it];
                mask_index :=  ifx c <= #char "Z" then
                    c - #char "A" + 26
                else
                    c - #char "a";

                mask |= cast(u64)(1 << mask_index);
            }

            return mask;
        }

        // compute masks
        mask_a := calculate_mask(a);
        mask_b := calculate_mask(b);
        mask_c := calculate_mask(c);

        // intersect masks
        intersect := mask_a & mask_b & mask_c;

        // extract bit
        bit := bit_scan_forward(intersect);
        value := cast(u64)bit;
        result += value;
    }

    return result;
}

// --------------------------------------------------------
// Day 04
// --------------------------------------------------------
day04 :: () {
    day04_test();
    day04_solve();
}

day04_test :: () {
    input, success := read_entire_file("data/day04_example.txt");
    assert(success);
    
    part1, part2 := day04_bothparts(input);
    assert(part1 == 2);
    assert(part2 == 4);
}

day04_solve :: () {
    input, success := read_entire_file("data/day04.txt");
    assert(success);

    part1, part2 := day04_bothparts(input);
    print("day04 part1: %\n", part1);
    print("day04 part1: %\n", part2);
}

day04_bothparts :: (input : string) -> u64, u64 {
    part1 : u64 = 0;
    part2 : u64 = 0;

    while input.count > 0 {
        line := next_line(*input);
        
        elves := split(line,",");
        assert(elves.count == 2);

        get_nums :: (s : string) -> int, int {
            range := split(s, "-");
            assert(range.count == 2);
            return string_to_int(range[0]), string_to_int(range[1]);
        }

        a,b := get_nums(elves[0]);
        x,y := get_nums(elves[1]);

        contains :: (a:int, b:int, x:int, y: int) -> bool {
            return (a >= x && b <= y) || (x >= a && y <= b);
        }

        overlaps :: (a:int, b:int, x:int, y: int) -> bool {
            return a <= y && x <= b;
        }
        
        if contains(a,b,x,y) part1 += 1;
        if overlaps(a,b,x,y) part2 += 1;
    }

    return part1, part2;
}

#import "Basic";
#import "File";
#import "String";
#import "Sort";
#import "Bit_Operations";