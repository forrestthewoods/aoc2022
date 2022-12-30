main :: () {
    start_time := get_time();
    defer {
        elapsed := get_time() - start_time;
        log("% ms elapsed.\n", elapsed * 1000);
    }

    // day01();
    // day02();
    // day03();
    // day04();
    // day05();
    // day06();
    // day07();
    // day08();
    // day09();
    // day10();
    // day11();
    // day12();
    // day13();
    // day14();
    // day15();
    // day16();
    // day17();
    // day18();
    // day19();
    day20();
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

heap_push :: (arr: *[..]$T, v: T, f: (T,T) -> bool) {
    // add to end
    array_add(arr, v);
    if arr.count == 1 return;

    // index of new element
    idx := arr.count - 1;

    // bubble up to top
    while idx > 0 {
        parent_idx := (idx-1)/2;
        if f(arr.data[idx], arr.data[parent_idx]) {
            Swap(*arr.data[idx], *arr.data[parent_idx]);
            idx = parent_idx;
        } else {
            break;
        }
    }
}

heap_pop :: (arr: *[..]$T, f: (T,T) -> bool) -> T {
    assert(arr.count > 0);
    result : T = arr.data[0];
    
    // swap first and last element
    Swap(*arr.data[0], *arr.data[arr.count - 1]);
    
    // remove last element
    arr.count -= 1;

    // re-heapify
    heapify(<<arr, 0, f);

    // return value
    return result;
}

heapify :: (arr: [..]$T, i: int, f: (T,T) -> bool) {
    largest := i;
    l := 2*i + 1;
    r := 2*i + 2;

    if l < arr.count && f(arr[l], arr[largest]) largest = l;
    if r < arr.count && f(arr[r], arr[largest]) largest = r;
    if largest != i {
        Swap(*arr[i], *arr[largest]);
        heapify(arr, largest, f);
    }
}

xprint :: (args: ..Any) -> string {
    builder: String_Builder; defer free_buffers(*builder);
    for args print_to_builder(*builder, tprint("%", it));
    return builder_to_string(*builder);
}

Vec2 :: struct {
    x : int;
    y : int;
}

make_vec2 :: (x: int, y: int) -> Vec2 {
    result: Vec2;
    result.x = x;
    result.y = y;
    return result;
}

add :: (a: Vec2, b: Vec2) -> Vec2 {
    result : Vec2;
    result.x = a.x + b.x;
    result.y = a.y + b.y;
    return result;
}

subtract :: (a: Vec2, b: Vec2) -> Vec2 {
    result : Vec2;
    result.x = a.x - b.x;
    result.y = a.y - b.y;
    return result;
}

manhatten_length :: (v: Vec2) -> int {
    return Math.abs(v.x) + Math.abs(v.y);
}

operator == :: (a: Vec2, b: Vec2) -> bool {
    return a.x == b.x && a.y == b.y;
}

get_hash :: (v: Vec2) -> u32 {
    Hash :: #import "Hash";
    return hash_combine(Hash.get_hash(v.x), Hash.get_hash(v.y));
}

Vec3 :: struct {
    x: int;
    y: int;
    z: int;
}

operator + :: (a: Vec3, b: Vec3) -> Vec3 {
    return Vec3.{
        a.x + b.x,
        a.y + b.y,
        a.z + b.z 
    };
}

operator == :: (a: Vec3, b: Vec3) -> bool {
    return a.x == b.x && a.y == b.y && a.z == b.z;
}

get_hash_vec3 :: (v: Vec3) -> u32 {
    Hash :: #import "Hash";
    return hash_combine(
            hash_combine(Hash.get_hash(v.x), Hash.get_hash(v.y)),
            Hash.get_hash(v.z));
}

hash_combine :: (seed: u32, v: u32) -> u32 {
    return seed ^ (v + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}

slice :: (s: string, start: s64) -> string {
    assert(start >= 0 && start < s.count);
    
    result := s;
    result.data += start;
    result.count -= start;
    return result;
}

slice :: (arr: [] $T, start: s64) -> [] T {
    assert(start < arr.count);
    result := arr;
    result.data += start;
    result.count -= start;
    return result;
}

vec2_to_idx :: (v: Vec2, width: int) -> int {
    return v.y*width + v.x;
}

idx_to_vec2 :: (idx: int, width: int) -> Vec2 {
    v : Vec2;
    v.x = idx % width;
    v.y = idx / width;
    return v;
}

parse_vec2 :: (s: string) -> Vec2 {
    parts := split(s, ",");
    assert(parts.count == 2);

    result : Vec2;
    result.x = string_to_int(parts[0]);
    result.y = string_to_int(parts[1]);
    return result;
}

array_contains :: (array: [] $T, item: T) -> bool {
    success, idx := array_find(array, item);
    return success;
}

find_or_add :: (table: *Table, key: table.Key_Type, default_value: table.Value_Type) -> *table.Value_Type {
    value := table_find_pointer(table, key);
    if value return value;

    value = table_add(table, key, default_value);
    return value;
}


table_contains :: (table: *Table, key: table.Key_Type) -> bool {
    v, success := table_find(table, key);
    return success;  
}

table_find_assert :: (table: *Table, key: table.Key_Type) -> table.Value_Type {
    v, success := table_find(table, key);
    assert(success);
    return v;  
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

// --------------------------------------------------------
// Day 05
// --------------------------------------------------------
day05 :: () {
    day05_test();
    day05_actual();
}

day05_test :: () {
    input, success := read_entire_file("data/day05_example.txt");
    assert(success);

    part1 := day05_solve(input, true);
    assert(part1 == "CMZ");

    part2 := day05_solve(input, false);
    assert(part2 == "MCD");
}

day05_actual :: () {
    input, success := read_entire_file("data/day05.txt");
    assert(success);

    part1 := day05_solve(input, true);
    assert(part1 == "ZRLJGSCTR");
    print("day05 part1: %\n", part1);

    part2 := day05_solve(input, false);
    assert(part2 == "PRTTGRFPB");
    print("day05 part2: %\n", part2);
}

day05_solve :: (input: string, is_part1: bool) -> string {
    // divide input into "inital stacks" and "moves"
    input_parts := split(input, "\r\n\r\n");
    assert(input_parts.count == 2);

    // split initial stacks into rows
    rows := split(input_parts[0], "\r\n");
    rows.count -= 1; // ignore last line which are just ids

    stacks : [..][..]u8;

    // parse stacks row by row
    for row : rows {
        idx := 0;
        col := 0;
        while idx < row.count {
            defer col += 1;
            defer idx += 4;

            if row[idx] == #char "[" {
                value := row[idx+1];
                
                // Ensure we have enough stacks
                if stacks.count <= col {
                    array_resize(*stacks, col + 1);
                }

                // Insert into stack
                array_insert_at(*stacks[col], value, 0);
            }       
        }
    }


    // process each move
    moves := split(input_parts[1], "\r\n");
    for move : moves {
        // format: move # from # to #
        move_parts := split(move, " ");
        assert(move_parts.count == 6);
        count := string_to_int(move_parts[1]);

        // get stacks (1-based index)
        src := string_to_int(move_parts[3]) - 1;
        dst := string_to_int(move_parts[5]) - 1;
        src_stack : *[..]u8 = *stacks[src];
        dst_stack : *[..]u8 = *stacks[dst];

         if is_part1 {
            // part1: move from src to dst one-by-one
            for 0..count-1 {
                array_add(dst_stack, pop(src_stack));
            }
        }
        else {
            // part2: move from src to dst in chunk
            bottom := src_stack.count - count;
            for bottom..src_stack.count - 1 {
                idx := it;
                value : u8 = src_stack.data[idx];
                array_add(dst_stack, value);
            }
            src_stack.count -= count;
        }
    }

    // extract top values for part1
    answer : [..]u8;
    for stack : stacks {
        if stack.count > 0 {
            array_add(*answer, peek(stack));
        }
    }
    return to_string(answer);
}

// --------------------------------------------------------
// Day 06
// --------------------------------------------------------
day06_part1_len :: 4;
day06_part2_len :: 14;

day06 :: () {
    day06_test();
    day06_actual();
}

day06_test :: () {
    assert(day06_solve("mjqjpqmgbljsphdztnvjfqwrcgsmlb", day06_part1_len) == 7);
    assert(day06_solve("bvwbjplbgvbhsrlpgdmjqwftvncz", day06_part1_len) == 5);
    assert(day06_solve("nppdvjthqldpwncqszvftbrmjlhg", day06_part1_len) == 6);
    assert(day06_solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", day06_part1_len) == 10);
    assert(day06_solve("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", day06_part1_len) == 11);

    assert(day06_solve("mjqjpqmgbljsphdztnvjfqwrcgsmlb", day06_part2_len) == 19);
    assert(day06_solve("bvwbjplbgvbhsrlpgdmjqwftvncz", day06_part2_len) == 23);
    assert(day06_solve("nppdvjthqldpwncqszvftbrmjlhg", day06_part2_len) == 23);
    assert(day06_solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", day06_part2_len) == 29);
    assert(day06_solve("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", day06_part2_len) == 26);
}

day06_actual :: () {
    input, success := read_entire_file("data/day06.txt");
    assert(success);

    part1 := day06_solve(input, day06_part1_len);
    assert(part1 == 1929);
    print("day06 part1: %\n", part1);

    part2 := day06_solve(input, day06_part2_len);
    assert(part2 == 3298);
    print("day06 part2: %\n", part2);
}

day06_solve :: (input: string, marker_len: int) -> s64 {
    for i: 0..input.count - 1 - marker_len {
        has_dupe := false;
        for a: i..(i+marker_len-1) {
            for b: (a+1)..(i+marker_len-1) {
                if input.data[a] == input.data[b] {
                    has_dupe = true;
                    break a;
                }
            }
        }

        if !has_dupe {
            return i + marker_len;
        }
    }

    assert(false);
    return 0;
}

// --------------------------------------------------------
// Day 07
// --------------------------------------------------------
d7_directory :: struct {
    parent: *d7_directory;
    name : string;
    files: [..]d7_file;
    children: [..]d7_directory;
    recursive_size : u64;
}

d7_file :: struct {
    size: u64;
    name: string;
}

d7_find_child :: (dir: *d7_directory, child_name: string) -> *d7_directory {
    for *child : dir.children {
        if child.name == child_name {
            return child;
        }
    }

    return null;
}


day07 :: () {
    day07_test();
    day07_actual();
}

day07_test :: () {
    input, success := read_entire_file("data/day07_example.txt");
    assert(success);

    part1, part2 := day07_solve(input);
    assert(part1 == 95437);
    assert(part2 == 24933642);
}

day07_actual :: () {
    input, success := read_entire_file("data/day07.txt");
    assert(success);

    part1, part2 := day07_solve(input);
    assert(part1 == 1454188);
    assert(part2 == 4183246);

    print("day07 part1: %\n", part1);
    print("day07 part2: %\n", part2);
}


day07_solve :: (input: string) -> u64, u64 {
    // build directory structure
    lines := split(input, "\r\n");
    assert(lines[0] == "$ cd /");

    root : d7_directory;
    root.name = "/";

    cur : *d7_directory;
    
    line_idx := 0;
    while line_idx < lines.count {
        line := lines[line_idx];
        line_idx += 1;

        if line == "$ cd /" {
            // move to root
            cur = *root;
        } else if line == "$ cd .." {
            // move to parent
            if cur.parent {
                cur = cur.parent;
            }
        }
        else if starts_with(line, "$ cd ") {
            parts := split(line, " ");
            assert(parts.count == 3);

            target_dir := parts[2];

            // Look for child
            child := d7_find_child(cur, target_dir);
            
            if !child {
                // haven't scanned this directory yet, create child
                child_dir : d7_directory;
                child_dir.parent = cur;
                child_dir.name = target_dir;
                array_add(*cur.children, child_dir);

                cur = *cur.children.data[cur.children.count - 1];
            } else {
                cur = child;
            }
        }
        else if line == "$ ls" {
            // list directory
            if cur.children.count != 0 || cur.files.count != 0 {
                // don't populate directory twice
                assert(false);
            } else {

                while line_idx < lines.count && !starts_with(lines[line_idx], "$") {
                    line = lines[line_idx];
                    line_idx += 1;
                
                    // split line
                    parts := split(line, " ");
                    assert(parts.count == 2);
                    a := parts[0];
                    b := parts[1];

                    if a == "dir" {
                        child_dir : d7_directory;
                        child_dir.parent = cur;
                        child_dir.name = b;
                        array_add(*cur.children, child_dir);
                    } else {
                        // file
                        file : d7_file;
                        file.size = cast(u64)string_to_int(a);
                        file.name = b;
                        array_add(*cur.files, file);
                    }
                }
            }
        } else {
            assert(false);
        }
    }

    // pre-comptue recursive directory size
    day07_calc_recursive_sizes(*root);

    // compute puzzle answers
    part1 := day07_calc_part1(*root);
    part2 := day07_calc_part2(*root);
    return part1, part2;
}

day07_calc_recursive_sizes :: (dir: *d7_directory) {
    for *child : dir.children {
        day07_calc_recursive_sizes(child);
        dir.recursive_size += child.recursive_size;
    }

    for *file : dir.files {
        dir.recursive_size += file.size;
    }
}

day07_calc_part1 :: (dir: *d7_directory) -> u64 {
    result : u64 = 0;

    if dir.recursive_size <= 100000 {
        result += dir.recursive_size;
    }

    for child : dir.children {
        result += day07_calc_part1(*child);
    }

    return result;
}

day07_calc_part2 :: (dir: *d7_directory) -> u64 {
    total_space :: 70000000;
    space_needed :: 30000000;

    space_used := dir.recursive_size;
    
    space_available := total_space - space_used;
    space_to_free := space_needed - space_available;

    smallest : u64 = total_space;
    find_smallest :: (d: *d7_directory, smallest: *u64, minimum: u64) {
        size : u64 = d.recursive_size;
        s : u64 = <<smallest;
        if size >= minimum && size < s {
            <<smallest = size;
        }

        for *child : d.children {
            find_smallest(child, smallest, minimum);
        }
    }

    find_smallest(dir, *smallest, space_to_free);

    return smallest;
}

// --------------------------------------------------------
// Day 08
// --------------------------------------------------------
day08 :: () {
    day08_test();
    day08_actual();
}

day08_test :: () {
    input, success := read_entire_file("data/day08_example.txt");
    assert(success);

    part1, part2 := day08_solve(input);
    assert(part1 == 21);
    assert(part2 == 8);
}

day08_actual :: () {
    input, success := read_entire_file("data/day08.txt");
    assert(success);

    part1, part2 := day08_solve(input);
    assert(part1 == 1546);
    print("day08 part1: %\n", part1);
    assert(part2 == 519064);
    print("day08 part2: %\n", part2);
}

day08_solve :: (input: string) -> int, int {
    trees := split(input, "\r\n");
    num_rows := trees.count;
    num_cols := trees[0].count;

    get_height :: (trees: []string, row: int, col: int) -> u8 {
        return trees.data[row].data[col];
    }

    // initialize to exterior
    part1 : int = num_rows*2 + (num_cols - 2) * 2;
    
    part2 := 0;
    
    // iterate interior
    for row: 1..num_rows - 2 {
        for col : 1..num_cols - 2 {
            // check if it's visible
            is_visible :: (trees : []string, row: int, col: int) -> bool {
                num_rows := trees.count;
                num_cols := trees[0].count;
                tree_height := get_height(trees, row, col);
                
                // check up
                visible := true;
                for r: 0.. row-1 {
                    if get_height(trees, r, col) >= tree_height {
                        visible = false;
                        break;
                    }
                }
                if visible return true;

                // check down
                visible = true;
                for r: row+1..num_rows-1 {
                    if get_height(trees, r, col) >= tree_height {
                        visible = false;
                        break;
                    }
                }
                if visible return true;

                // check left
                visible = true;
                for c: 0..col-1 {
                    if get_height(trees, row, c) >= tree_height {
                        visible = false;
                        break;
                    }
                }
                if visible return true;

                // check right
                visible = true;
                for c: col+1..num_cols-1 {
                    if get_height(trees, row, c) >= tree_height {
                        visible = false;
                        break;
                    }
                }
                if visible return true;

                return false;
            }

            calc_scenic_score :: (trees : []string, row: int, col: int) -> int {
                score := 1;

                num_rows := trees.count;
                num_cols := trees[0].count;
                tree_height := get_height(trees, row, col);

                // look up
                dir_trees := 0;
                r := row - 1;
                while r >= 0 {
                    defer r -= 1;
                    dir_trees += 1;

                    if get_height(trees, r, col) >= tree_height {
                        break;
                    }
                }
                score *= dir_trees;

                // look down
                dir_trees = 0;
                r = row + 1;
                while r < num_rows  {
                    defer r += 1;
                    dir_trees += 1;

                    if get_height(trees, r, col) >= tree_height {
                        break;
                    }
                }
                score *= dir_trees;
                
                // look left
                dir_trees = 0;
                c := col - 1;
                while c >= 0 {
                    defer c -= 1;
                    dir_trees += 1;

                    if get_height(trees, row, c) >= tree_height {
                        break;
                    }
                }
                score *= dir_trees;

                // look right
                dir_trees = 0;
                c = col + 1;
                while c < num_cols {
                    defer c += 1;
                    dir_trees += 1;

                    if get_height(trees, row, c) >= tree_height {
                        break;
                    }
                }
                score *= dir_trees;
                
                return score;
            }

            if is_visible(trees, row, col) {
                part1 += 1;
            }

            maybe_part2 := calc_scenic_score(trees, row, col);
            part2 = max(maybe_part2, part2);
        }
    }

    return part1, part2;
}

// --------------------------------------------------------
// Day 09
// --------------------------------------------------------
day09 :: () {
    day09_test();
    day09_actual();
}

day09_test :: () {
    input, success := read_entire_file("data/day09_example.txt");
    assert(success);

    assert(day09_solve(input, 2) == 13);
    assert(day09_solve(input, 10) == 1);

    input, success = read_entire_file("data/day09_example2.txt");
    assert(success);
    assert(day09_solve(input, 10) == 36);
}

day09_actual :: () {
    input, success := read_entire_file("data/day09.txt");
    assert(success);

    part1 := day09_solve(input, 2);
    assert(part1 == 6197);
    print("day09 part1: %\n", part1);

    part2 := day09_solve(input, 10);
    assert(part2 == 2562);
    print("day09 part1: %\n", part2);
}

day09_solve :: (input: string, rope_len: int) -> int {
    rope : [..]Vec2;
    array_resize(*rope, rope_len);

    UP :: 0;
    DOWN :: 1;
    LEFT :: 2;
    RIGHT :: 3;

    // This is clunky
    dirs : [4]Vec2;
    dirs[UP].y = -1;
    dirs[DOWN].y = 1;
    dirs[LEFT].x = -1;
    dirs[RIGHT].x = 1;

    // Initialize HashTable (it's a set) for tracking visited positions
    visited : Table(Vec2, int, get_hash);
    init(*visited, 256);
    table_set(*visited, Vec2.{}, 0);

    move := 0;
    for line : split(input, "\r\n") {
        parts := split(line, " ");
        assert(parts.count == 2);

        dir := parts[0].data[0];
        amount := string_to_int(parts[1]);
        
        // get velocity
        vel : Vec2;
        if dir == #char "U" vel = dirs[UP];
        else if dir == #char "D" vel = dirs[DOWN];
        else if dir == #char "L" vel = dirs[LEFT];
        else if dir == #char "R" vel = dirs[RIGHT];

        for 0..amount -1 {
            // advance head
            rope[0] = add(rope[0], vel);

            // wag the tail
            for head_idx: 0..rope.count - 2 {
                tail_idx := head_idx + 1;

                diff := subtract(rope[head_idx], rope[tail_idx]);
                if Math.abs(diff.x) > 1 || Math.abs(diff.y) > 1 {
                    diff.x = clamp(diff.x, -1, 1);
                    diff.y = clamp(diff.y, -1, 1);
                    rope[tail_idx] = add(rope[tail_idx], diff);
                }
            }
            
            table_set(*visited, rope[rope.count - 1], 0);
        }
    }    

    return visited.count;
}

// --------------------------------------------------------
// Day 10
// --------------------------------------------------------
day10 :: () {
    day10_test();
    day10_actual();
}

day10_test :: () {
    input, success := read_entire_file("data/day10_example2.txt");
    assert(success);

    part1, part2 := day10_solve(input);
    assert(part1 == 13140);
}

day10_actual :: () {
    input, success := read_entire_file("data/day10.txt");
    assert(success);

    part1, part2 := day10_solve(input);
    print("day10 part1: %\n", part1);
    print("day10 part2: \n%\n", part2);
}

day10_solve :: (input: string) -> int, string {
    part1 : int = 0;
    part2 : [..]u8;
    part2_row : [..]u8;

    // Breakup input
    instructions := split(input, "\r\n");

    // Create CPU
    cpu : d10_cpu;

    // Run until last cycle
    instruction_ptr : *string = *instructions[0];

    while cpu.next_cycle <= 240 {
        cur_cycle := cpu.next_cycle;

        if cpu.pipeline.cycles_left < 0 {
            // start next instruction
            if starts_with(<<instruction_ptr, "noop") {
                // go to next instruction
                instruction_ptr += 1;
            } else if starts_with(<<instruction_ptr, "addx") {
                parts := split(<<instruction_ptr, " ");
                assert(parts.count == 2);

                cpu.pipeline.type = parts[0];
                cpu.pipeline.value = string_to_int(parts[1]);
                cpu.pipeline.cycles_left = 2;
            } else {
                assert(false);
            }
        }

        // Process  part1
        if (cur_cycle - 20) % 40 == 0 {
            part1 += cur_cycle * cpu.register;
        }

        // process part2
        lit : u8 :  #char "#";
        unlit : u8 : #char " ";
        
        pixel : int = ((cur_cycle - 1) % 40);
        cursor : int = cpu.register;
        byte : u8 = unlit;
        if Math.abs(pixel - cursor) <= 1 {
            byte = lit;
        }
        array_add(*part2, byte);
        array_add(*part2_row, byte);
        if cur_cycle % 40 == 0 {
            array_add(*part2, #char "\n");
            array_reset(*part2_row);
        }

        if cur_cycle == 240 break;

        // Process end of cycle instruction operation
        if cpu.pipeline.cycles_left > 0 {
            // decrement cycle counter
            cpu.pipeline.cycles_left -= 1;
            
            if cpu.pipeline.cycles_left == 0 {
                // finish instruction
                assert(cpu.pipeline.type == "addx");
                cpu.pipeline.cycles_left = -1;

                // execute instruction
                cpu.register += cpu.pipeline.value;

                // increment instruction ptr
                instruction_ptr += 1;
            }
        } 
 

        // Increment cycle
        cpu.next_cycle += 1;
    }

    return part1, to_string(part2);
}

d10_cpu :: struct {
    next_cycle : int = 1;
    register : int = 1;
    pipeline: d10_instruction;
}

d10_instruction :: struct {
    type: string;
    value: int;
    cycles_left : int = -1;
}

// --------------------------------------------------------
// Day 11
// --------------------------------------------------------
day11 :: () {
    day11_test();
    day11_actual();
}

day11_test :: () {
    input, success := read_entire_file("data/day11_example.txt");
    assert(success);

    assert(day11_solve(input, false) == 10605);
    assert(day11_solve(input, true) == 2713310158);
}

day11_actual :: () {
    input, success := read_entire_file("data/day11.txt");
    assert(success);

    part1 := day11_solve(input, false);
    assert(part1 == 61005);
    print("day11 part1: %\n", part1);

    part2 := day11_solve(input, true);
    assert(part2 == 20567144694);
    print("day11 part2: %\n", part2);
}

day11_solve :: (input: string, part2: bool) -> int {
    monkey_strings := split(input, "\r\n\r\n");   
    assert(monkey_strings.count > 1);

    monkies : [..]d11_monkey;

    // parse 
    for monkey_str : monkey_strings {
        new_monkey : d11_monkey;

        monkey_lines := split(trim(monkey_str), "\r\n");
        assert(monkey_lines.count == 6);

        // extract items
        items_str := slice(monkey_lines[1], 18);
        for item : split(items_str, ", ") array_add(*new_monkey.items, string_to_int(item));
        
        // extract operation
        op_str := slice(monkey_lines[2], 23);
        if op_str == "* old" {
            new_monkey.op_type = d11_op.square;
        }
        else if starts_with(op_str, "+") {
            new_monkey.op_type = d11_op.add;
            new_monkey.op_value = string_to_int(slice(op_str, 2));
        } else if starts_with(op_str, "*") {
            new_monkey.op_type = d11_op.multiply;
            new_monkey.op_value = string_to_int(slice(op_str, 2));
        } else {
            assert(false);
        }

        // extrat test
        new_monkey.test_value = string_to_int(slice(monkey_lines[3], 21));

        // extract targets
        new_monkey.true_target = string_to_int(slice(monkey_lines[4], 29));
        new_monkey.false_target = string_to_int(slice(monkey_lines[5], 30));

        // store monkey
        array_add(*monkies, new_monkey);
    }

    num_rounds := ifx part2 then 9999 else 19;
    
    modulo_product := 1;
    for monkies modulo_product *= it.test_value;

    // run rounds
    for 0..num_rounds {
        // update monkey
        for *monkey : monkies {
            // process each item
            for item : monkey.items {
                // apply operation
                initial_value := item;
                if monkey.op_type == d11_op.add {
                    item += monkey.op_value;
                } else if monkey.op_type == d11_op.multiply {
                    item *= monkey.op_value;
                } else if monkey.op_type == d11_op.square {
                    item *= item;
                } else { assert(false); }
                assert(item >= 0);

                // decrease worry (part1 only)
                if !part2 {
                    item /= 3;
                } else {
                    item = item % modulo_product;
                }

                // update counter
                monkey.items_inspected += 1;

                // throw to new monkey
                target := ifx item % monkey.test_value == 0 
                    then monkey.true_target
                    else monkey.false_target;
                
                array_add(*monkies[target].items, item);
            }

            // always throw all items
            monkey.items.count = 0;
        }
    }

    // sort
    compare :: (a: *d11_monkey, b: *d11_monkey) -> s64 {
        delta := a.items_inspected - b.items_inspected;
        if delta < 0 return  1;
        if delta > 0 return -1;
        return 0;
    }
    monkey_ptrs : [..]*d11_monkey;
    for *monkey : monkies array_add(*monkey_ptrs, monkey);
    sorted_monkies := quick_sort(monkey_ptrs, compare);

    // calc answer
    answer := sorted_monkies[0].items_inspected * sorted_monkies[1].items_inspected;
    return answer;
}

d11_op :: enum { add; multiply; square; };

d11_monkey :: struct {
    items : [..]s64;
    op_type : d11_op;
    op_value : int;
    test_value: int;
    true_target : int;
    false_target : int;
    items_inspected : int;
}

// --------------------------------------------------------
// Day 12
// --------------------------------------------------------
day12 :: () {
    day12_test();
    day12_actual();
}

day12_test :: () {
    input, success := read_entire_file("data/day12_example.txt");
    assert(success);

    assert(day12_solve(input, true) == 31);
    assert(day12_solve(input, false) == 29);
}

day12_actual :: () {
    input, success := read_entire_file("data/day12.txt");
    assert(success);

    part1 := day12_solve(input, true);
    assert(part1 == 352);
    print("day12 part1: %\n", part1);

    part2 := day12_solve(input, false);
    assert(part2 == 345);
    print("day12 part2: %\n", part2);
}

day12_solve :: (input: string, part1: bool) -> int {
    map : [..]u8;

    // build map
    lines := split(input, "\r\n");
    assert(lines.count > 0);
    width := lines[0].count;
    height := lines.count;

    for line : lines {
        for line_idx: 0..line.count - 1 {
            array_add(*map, line[line_idx]);
        }
    }

    if part1 {
        // find start
        success, start_idx := array_find(map, #char "S");
        assert(success);

        has_solution, part1 := d12_solve(map, width, height, start_idx);
        assert(has_solution);
        return part1;
    } else {
        part2 : int = 2147483647;  

        for start_idx: 0..map.count-1 {
            if map[start_idx] == #char "a" {
                has_solution, maybe_part2 := d12_solve(map, width, height, start_idx);
                if has_solution {
                    part2 = min(part2, maybe_part2);
                }
            }
        }

        return part2;
    }
}

d12_solve :: (map: []u8, width: int, height: int, start_idx: int) -> bool, int {
    // helper data
    offsets : [4]Vec2;
    offsets[0] = Vec2.{-1,0};
    offsets[1] = Vec2.{1,0};
    offsets[2] = Vec2.{0,-1};
    offsets[3] = Vec2.{0,1};

    // initialize simple search
    open_list : [..]d12_entry;
    visited : [..]int;

    initial_entry : d12_entry;
    initial_entry.pos = idx_to_vec2(start_idx, width);
    array_add(*open_list, initial_entry);
    
    // climb the hill
    while open_list.count > 0 {
        entry := pop(*open_list);
        entry_idx := vec2_to_idx(entry.pos, width);
        entry_height := map[entry_idx];
        
        // check for summit
        if entry_height == #char "E" {
            answer := entry.steps;
            return true, answer;
        }
        
        // correct for starting position
        if entry_height == #char "S" entry_height = #char "a";

        // don't re-visit positions
        if array_contains(visited, entry_idx) {
            continue;
        }
        array_add(*visited, entry_idx);
        assert(visited.count <= map.count); 

        // check neighbors
        for offset : offsets {
            // calc neighbor pos
            n_pos := add(entry.pos, offset);
            
            // must be within bounds
            if n_pos.x < 0 || n_pos.x >= width || n_pos.y < 0 || n_pos.y >= height {
                continue;
            }

            // get neighbor height, correct for summit
            neighbor_height := map[vec2_to_idx(n_pos, width)];
            if neighbor_height == #char "E" {
                neighbor_height = #char "z" + 1;
            }

            // neighbor must be "at most one higher"
            if neighbor_height > entry_height + 1 {
                continue;
            }

            // create new entry
            new_entry : d12_entry;
            new_entry.pos = n_pos;
            new_entry.steps = entry.steps + 1;

            // add it to the front
            // gross and inefficient, oh well
            array_insert_at(*open_list, new_entry, 0);
        }
    }

    return false, 0;
}

d12_entry :: struct {
    pos : Vec2;
    steps : int;
}

// --------------------------------------------------------
// Day 13
// --------------------------------------------------------
day13 :: () {
    day13_test();
    day13_actual();
}

day13_test :: () {
    input, success := read_entire_file("data/day13_example.txt");
    assert(success);

    part1, part2 := day13_solve(input);
    assert(part1 == 13);
    assert(part2 == 140);
}

day13_actual :: () {
    input, success := read_entire_file("data/day13.txt");
    assert(success);

    part1, part2 := day13_solve(input);
    print("day13 part1: %\n", part1);
    assert(part1 == 6070);
    print("day13 part2: %\n", part2);
    assert(part2 == 20758);
}

day13_solve :: (input: string) -> int, int {
    pairs := split(input, "\r\n\r\n");
    assert(pairs.count > 1);

    // parse into lines
    parsed := day13_parse(input);
    assert(parsed.count % 2 == 0);


    // Part1: Iterate pairs and check sorted
    part1 := 0;
    parsed_idx := 0;
    while parsed_idx < parsed.count {
        defer parsed_idx += 2;

        a := parsed[parsed_idx];
        b := parsed[parsed_idx + 1];

        pair_idx := parsed_idx / 2 + 1;
        ordered := day13_compare(a, b);
        assert(ordered == 1 || ordered == -1);

        if ordered == 1 {
            part1 += pair_idx;
        }
    }

    // Part2: Sort list, find certain things
    is_divider :: (e: [..]d13_element) -> bool {
        if e.count == 1 {
            inner := e.data[0];
            if inner.nested.count == 1 {
                v := inner.nested[0].number;
                if v == 2 || v == 6 {
                    return true;
                }
            }
        }

        return false;
    }

    // inject dividers
    // this might be the ugliest code I've ever written for AoC
    {
        divider : [..]d13_element;
        inner : d13_element;
        inner_value := d13_element.{number = 2};
        array_add(*inner.nested, inner_value);
        array_add(*divider, inner);
        array_add(*parsed, divider);
    }
        {
        divider : [..]d13_element;
        inner : d13_element;
        inner_value := d13_element.{number = 6};
        array_add(*inner.nested, inner_value);
        array_add(*divider, inner);
        array_add(*parsed, divider);
    }

    inv_sort :: (a: [..]d13_element, b: [..]d13_element) -> int {
        return day13_compare(a,b) * -1;
    }

    sorted := quick_sort(parsed, inv_sort);
    part2 := 1;
    for sorted {
        if is_divider(it) {
            part2 *= it_index + 1;
        }
    }
    
    return part1, part2;
}

day13_compare :: (a : [..]d13_element, b: [..]d13_element) -> int {
    a_idx := 0;
    b_idx := 0;

    is_number :: (e: d13_element) -> bool {
        return e.number != -1;
    }

    while true {
        defer a_idx += 1;
        defer b_idx += 1;
        
        if a_idx >= a.count && b_idx < b.count return 1;
        else if b_idx >= b.count && a_idx < a.count return -1;
        else if a_idx >= a.count && b_idx >= b.count return 0;

        na := is_number(a[a_idx]);
        nb := is_number(b[b_idx]);

        if na && nb {
            // Both numbers
            va := a[a_idx].number;
            vb := b[b_idx].number;

            //print("compare % vs %\n", va, vb);
            if va < vb return 1;
            if vb < va return -1;
        } else if !na && !nb {
            //print("compare nested vs nested\n");
            maybe_result := day13_compare(a[a_idx].nested, b[b_idx].nested);
            if maybe_result != 0 return maybe_result;
        } else {
            // Mixed
            //print("mixed types\n");
            if na {
                fake_list : [..]d13_element;
                array_add(*fake_list, a[a_idx]);
                maybe_result := day13_compare(fake_list, b[b_idx].nested);
                if maybe_result != 0 return maybe_result;
            } else if nb {
                fake_list : [..]d13_element;
                array_add(*fake_list, b[b_idx]);
                maybe_result := day13_compare(a[a_idx].nested, fake_list);
                if maybe_result != 0 return maybe_result;
            } else assert(false);
        }
    }

    assert(false);
    return 0;
}

day13_parse :: (input : string) -> [..][..]d13_element {
    result : [..][..]d13_element;

    for pair : split(input, "\r\n\r\n") {
        for line : split(pair, "\r\n") {
            parsed_row : [..]d13_element = day13_parseElement(line);
            array_add(*result, parsed_row);
        }
    }

    return result;
}

day13_parseElement :: (line: string) -> [..]d13_element {
    assert(line[0] == #char"[");
    assert(line[line.count - 1] == #char"]");

    find_digits :: (start: *u8) -> string {
        assert(is_digit(<<start));

        idx := 0;
        while is_digit(start[idx]) idx += 1;

        result : string;
        result.data = start;
        result.count = idx;
        return result;
    }

    find_span :: (s: *u8) -> string {
        assert(s[0] == #char "[");
        
        num_open := 0;
        idx := 0;
        while true {
            // Increment
            if s[idx] == #char"[" num_open += 1;
            else if s[idx] == #char"]" num_open -= 1;
            assert(num_open >= 0);
            
            // Check for end
            if num_open == 0 {
                result : string;
                result.data = s;
                result.count = idx + 1;
                return result;
            }

            idx += 1;
        }

        assert(false);
        return "";
    }

    result : [..]d13_element;

    idx := 1;
    while idx < line.count - 1{
        if line[idx] == #char"," {
            idx += 1;
        }
        else if is_digit(line[idx]) {
            digits := find_digits(line.data + idx);
            number := string_to_int(digits);

            new_element : d13_element;
            new_element.number = number;
            array_add(*result, new_element);

            idx += digits.count + 1;
        }
        else {
            assert(line[idx] == #char"[");

            subspan := find_span(line.data + idx);

            new_element : d13_element;
            new_element.nested = day13_parseElement(subspan);
            array_add(*result, new_element);

            idx += subspan.count + 1;
        }
    }

    return result;
}

d13_element :: struct {
    nested : [..]d13_element;
    number : int = -1;
}

// --------------------------------------------------------
// Day 14
// --------------------------------------------------------
day14 :: () {
    day14_test();
    day14_actual();
}

day14_test :: () {
    input, success := read_entire_file("data/day14_example.txt");
    assert(success);

    part1 := day14_solve(input, true);
    assert(part1 == 24);

    part2 := day14_solve(input, false);
    assert(part2 == 93);
}

day14_actual :: () {
    input, success := read_entire_file("data/day14.txt");
    assert(success);

    part1 := day14_solve(input, true);
    assert(part1 == 698);
    print("day14 part1: %\n", part1);

    part2 := day14_solve(input, false);
    print("day14 part2: %\n", part2);
    assert(part2 == 28594);
}

day14_solve :: (input: string, part1: bool) -> int {
    // constants
    spawn_pos :: Vec2.{500, 0};


    // parse
    map : Table(Vec2, int, get_hash);
    init(*map, 1);

    for line : split(input, "\r\n") {
        parts := split(line, " -> ");
        assert(parts.count >= 2);

        start := parse_vec2(parts[0]);

        for part_idx: 1..parts.count - 1 {
            end := parse_vec2(parts[part_idx]);

            sx := min(start.x, end.x);
            ex := max(start.x, end.x);
            sy := min(start.y, end.y);
            ey := max(start.y, end.y);

            for x:sx..ex {
                for y:sy..ey {
                    pos := make_vec2(x,y);
                    table_set(*map, pos, 1);
                }
            }

            start = end;
        }
    }

    // find low point (positive down)
    max_y := 0;
    for map {
        key, value := it_index, it;
        max_y = max(max_y, key.y);
    }

    // Infinite floor for part2
    if !part1 {
        max_y += 2;   
        sx := -(max_y * 3);
        ex := (max_y * 3);
        
        for x: sx..ex {
            pos := make_vec2(spawn_pos.x + x,max_y);
            table_set(*map, pos, 1);
        }
    }

    spot_filled :: (t: *Table, k: Vec2) -> bool {
        value, success := table_find(t, k);
        return success;
    }

    // spawn sand from the ceiling
    num_sands := 0;
    sand_pos : Vec2;
    while sand_pos.y <= max_y {
        // spawn a new grain of sand
        sand_pos = spawn_pos;
        while sand_pos.y <= max_y {
            down := add(sand_pos, Vec2.{0,1});
            down_left := add(sand_pos, Vec2.{-1,1});
            down_right := add(sand_pos, Vec2.{1,1});
            
            // move sand
            if !spot_filled(*map, down) sand_pos = down;
            else if !spot_filled(*map, down_left) sand_pos = down_left;
            else if !spot_filled(*map, down_right) sand_pos = down_right;
            else if sand_pos == spawn_pos {
                assert(!part1);
                return num_sands + 1;
            }
            else {
                table_set(*map, sand_pos, 1);
                num_sands += 1;
                break;
            }
        }
    }

    return num_sands;
}

// --------------------------------------------------------
// Day 15
// --------------------------------------------------------
day15 :: () {
    day15_test();
    day15_actual();
}

day15_test :: () {
    input, success := read_entire_file("data/day15_example.txt");
    assert(success);

    part1, part2 := day15_solve(input, 10, 20);
    assert(part1 == 26);
    assert(part2 == 56000011);
}

day15_actual :: () {
    input, success := read_entire_file("data/day15.txt");
    assert(success);

    part1, part2 := day15_solve(input, 2000000, 4000000);
    assert(part1 == 4879972);
    assert(part2 == 12525726647448);
    print("day15 part1: %\n", part1);
    print("day15 part2: %\n", part2);

    // part2 := day15_solve(input, false);
    // print("day15 part2: %\n", part2);
    // assert(part2 == 28594);
}

day15_solve :: (input: string, part1_row: int, part2_limit: int) -> int, u64 {
    // constants
    SENSOR : int : 1;
    BEACON : int : 2;
    SCANNED : int : 3;
    
    // parse data
    sensors : [..]Vec2;
    beacons : Table(Vec2, int, get_hash);
    radii : [..]int;

    min_x := 1_000_000_000;
    max_x := -1_000_000_000;

    for line : split(input, "\r\n") {
        parts := split(line, " ");
        assert(parts.count == 10);

        sensor := make_vec2(
            string_to_int(trim(parts[2], "x=,")),
            string_to_int(trim(parts[3], "y=,")));
        array_add(*sensors, sensor);

        beacon := make_vec2(
            string_to_int(trim(parts[8], "x=,")),
            string_to_int(trim(parts[9], "y=,")));
        table_set(*beacons, beacon, 1);

        array_add(*radii, manhatten_length(subtract(beacon, sensor)));

        min_x = min(min_x, beacon.x);
        max_x = max(max_x, beacon.x);
    }

    spans : [..]d15_span;

    merge_spans :: (spans: *[..]d15_span, in_span: d15_span) {
        new_span := in_span;
        
        idx := 0;
        while idx < spans.count {
            old_span := *spans.data[idx];

            if new_span.end < old_span.start {
                // new_span: |------|
                // old_span:            |------|
                array_insert_at(spans, new_span, idx);
                return;
            }
            else if new_span.start >= old_span.start && new_span.end <= old_span.end {
                // new_span:   |--|
                // old_span: |------|
                return;
            } else if new_span.start <= old_span.start && new_span.end >= old_span.end {
                // new_span: |------|
                // old_span:   |--|
                array_ordered_remove_by_index(spans, idx);
            }
            else if new_span.end <= old_span.end {
                // new_span: |------|
                // old_span:     |------|
                old_span.start = new_span.start;
                return;
            }
            else if new_span.start <= old_span.end {
                // new_span:         |------|
                // old_span:     |------|
                new_span.start = old_span.start;
                array_ordered_remove_by_index(spans, idx);
            } else if  new_span.start > old_span.end {
                // new_span:           |------|
                // old_span: |------|
                idx += 1;
            } else {
                assert(false);
            }            
        }

        array_add(spans, new_span);
    }

    build_spans :: (sensors: []Vec2, radii: []int, row: int) -> [..]d15_span {
        spans : [..]d15_span;
        
        // Create "map" of spans
        for idx: 0..sensors.count - 1 {
            sensor := sensors[idx];
            radius := radii[idx];

            y_diff := Math.abs(sensor.y - row);
            if y_diff <= radius {
                x_rem := radius - y_diff;

                new_span : d15_span;
                new_span.start = sensor.x - x_rem;
                new_span.end = sensor.x + x_rem;

                merge_spans(*spans, new_span);
            }
        }

        return spans;
    }
    

    in_spans :: (spans: [..]d15_span, v: int) -> bool {
        for span : spans {
            if v >= span.start && v <= span.end {
                return true;
            }
        }
        return false;
    }

    part1 := 0;
    {
        spans := build_spans(sensors, radii, part1_row);

        // Sum spans
        for span : spans part1 += span.end - span.start + 1;
        for beacons {
            beacon := it_index;
            if beacon.y == part1_row && in_spans(spans, beacon.x) {
                part1 -= 1;
            }
        }
    }

    part2 : u64 = 0;
    {
        for row: 0..part2_limit {
            spans := build_spans(sensors, radii, row);

            idx := 0;
            while idx < spans.count - 1 {
                defer idx += 1;

                lo := spans[idx];
                hi := spans[idx+1];
                x := lo.end + 1;
                if x == (hi.start - 1) {
                    part2 = cast(u64)x * 4000000 + cast(u64)row;
                    break row;
                }
            }
        }
    }

    return part1, part2;
}

d15_span :: struct {
    start : int;
    end : int;
}

// --------------------------------------------------------
// Day 16
// --------------------------------------------------------
day16 :: () {
    day16_test();
    day16_actual();
}

day16_test :: () {
    input, success := read_entire_file("data/day16_example.txt");
    assert(success);

    print("test part1\n");
    assert(day16_solve(input, true) == 1651);
    print("test part2\n");
    assert(day16_solve(input, false) == 1707);
}

day16_actual :: () {
    input, success := read_entire_file("data/day16.txt");
    assert(success);

    part1 := day16_solve(input, true);
    assert(part1 == 1845);
    print("day16 part1: %\n", part1);

    part2 := day16_solve(input, false);
    assert(part2 == 2286);
    print("day16 part2: %\n", part2);
}

day16_solve :: (input: string, part1: bool) -> int {
    room_to_mask : Table(RoomIndex, u64);
    valveroom_to_mask : Table(RoomIndex, u64);
    valverooms_mask : u64 = 0;

    room_to_index : Table(string, RoomIndex);
    room_idx : RoomIndex = 0;
    for line : split(input, "\r\n") {
        parts := split(line, " ");
        table_set(*room_to_index, parts[1], room_idx);
        room_idx += 1;
    }
    starting_room : string = "AA";
    starting_room_idx := table_find_assert(*room_to_index, starting_room);

    // parse input
    graph : [..]d16_room;
    for line : split(input, "\r\n") {
        parts := split(line, " ");

        new_room : d16_room;
        new_room.name = parts[1];
        room_index := table_find_assert(*room_to_index, new_room.name);
        
        new_room.mask = cast(u64)1 << room_to_mask.count;
        new_room.rate = string_to_int(trim(parts[4], "rate=;"));
        
        tunnels_parts := slice(parts, 9);
        for tunnel_part : tunnels_parts {
            tunnel := trim(tunnel_part, ",");
            array_add(*new_room.tunnels, table_find_assert(*room_to_index, tunnel));
        }

        if new_room.rate > 0 {
            table_set(*valveroom_to_mask, room_index, new_room.mask);
            valverooms_mask |= new_room.mask;
        }

        table_set(*room_to_mask, room_index, new_room.mask);
        array_add(*graph, new_room);
    }

    // BFS from each source to find paths between valves
    for *graph {
        init_room : *d16_room = it;
        //print("bfs for %\n", init_room.name);
        
        // help struct for search state
        bfs_state :: struct {
            room : RoomIndex;
            cost : int;
        }

        // initialize BFS search structs
        visited : Table(RoomIndex, int);
        open_list : [..]bfs_state;

        init_state : bfs_state;
        init_state.room = table_find_assert(*room_to_index, init_room.name);
        array_add(*open_list, init_state);
        table_set(*visited,  init_state.room, 0);

        // explore the whole thing
        while open_list.count > 0 {
            // find lowest
            lowest_cost := 100000;
            best_idx := 0;

            for *open_list {
                if it.cost < lowest_cost {
                    lowest_cost = it.cost;
                    best_idx = it_index;
                }
            }

            cur_state := open_list[best_idx];
            array_unordered_remove_by_index(*open_list, best_idx);

            graph_room : *d16_room = *graph.data[cur_state.room];
            
            // process each neighbor
            for neighbor : graph_room.tunnels {
                // don't visit rooms twice
                _, already_visited := table_find(*visited, neighbor);
                if already_visited continue;
                table_set(*visited, neighbor, 0);

                // create new state
                new_state : bfs_state;
                new_state.room = neighbor;
                new_state.cost = cur_state.cost + 1;

                // store paths to rooms with rate > 0
                neighbor_room : *d16_room = *graph.data[neighbor];
                if neighbor_room.rate > 0 {
                    table_set(*init_room.paths, neighbor, new_state.cost);
                }

                array_add(*open_list, new_state);
            }
        }
    }

    // bfs to find best rate
    full_list : Table(d16_full_key, [..]d16_search_state, get_hash_d16_full_key);
    open_list : [..]d16_search_state;
    scratch_rooms : [..]*d16_room;
    
    init_state : d16_search_state;
    init_state.cur_room = starting_room_idx;

    // part2 start with less time
    if !part1 {
        init_state.time_left = 26;
    }

    // init search states
    array_add(*open_list, init_state);
    // TODO: initialize full_list with initial state

    open_list_cmp :: (a: d16_search_state, b: d16_search_state) -> bool {
        return a.heuristic > b.heuristic;
    }

    // full traversal
    while open_list.count > 0 {
        // get current room
        //cur_state := pop(*open_list);
        cur_state := heap_pop(*open_list, open_list_cmp);
        assert(cur_state.time_left > 0);

        if cur_state.opened_valves == valverooms_mask {
            // Fully explored. Maybe da best?
            print("found da best [%]\n", cur_state);
            break;
        }
        
        cur_room := *graph.data[cur_state.cur_room];
        assert(cur_room != null);

        // consider each other valve
        for cur_room.paths {
            // get neighbor info
            neighbor : RoomIndex = it_index;
            neighbor_cost : int = it;

            neighbor_room := *graph.data[neighbor];
            assert(neighbor_room != null);
            assert(neighbor_room.rate > 0);

            // don't open valves multiple times
            if cur_state.opened_valves & neighbor_room.mask > 0 continue;

            // don't open if there's not enough time to move, open, and release
            cost_to_open := neighbor_cost + 1;
            if cur_state.time_left <= cost_to_open continue;

            // potential new state
            new_state : d16_search_state = cur_state;
            new_state.cur_room = neighbor;
            new_state.time_left = cur_state.time_left - cost_to_open;
            new_state.opened_valves = cur_state.opened_valves | neighbor_room.mask;
            
            pressure_to_release := neighbor_room.rate * new_state.time_left;
            new_state.released_pressure = cur_state.released_pressure + pressure_to_release;

            //new_state.solution_path = tprint("% %(%)", cur_state.solution_path, neighbor, substate_idx);
            //new_state.debug_moves = tprint("%opened [%]. after minute [%] time left: [%] valve_release: [%] substate: [%] sum_release: [%]\n", cur_state.debug_moves, neighbor, 30 - cur_substate.time_left, new_substate.time_left, pressure_to_release, substate_idx, new_state.released_pressure);

            key : d16_full_key;
            key.cur_room = new_state.cur_room;
            key.time_left = new_state.time_left;
            key.first_pawn = new_state.first_pawn;

            default_value : [..]d16_search_state;
            prev_states : *[..]d16_search_state = find_or_add(*full_list, key, default_value);

            // see if any visited state is "better" than new_state
            found_better := false;
            for *prev_state : <<prev_states {
                was_better :: (prev: d16_search_state, new: d16_search_state) -> bool {
                    if prev.released_pressure < new.released_pressure return false;
                    if (prev.opened_valves & new.opened_valves) != new.opened_valves return false;
                    
                    assert(prev.time_left == new.time_left);
                    assert(prev.cur_room == new.cur_room);

                    return true;
                }

                if was_better(prev_state, new_state) {
                    found_better = false;
                    break;
                }
            } 

            if !found_better {
                new_state.heuristic = d16_calc_heuristic(
                    new_state,
                    *scratch_rooms,
                    valveroom_to_mask,
                    graph,
                    part1);
                
                // store new state
                heap_push(*open_list, new_state, open_list_cmp);
                array_add(prev_states, new_state);
            }
        }

        // start second search
        if !part1 && cur_state.first_pawn {
            new_state := cur_state;
            new_state.time_left = 26;
            new_state.cur_room = starting_room_idx;
            new_state.first_pawn = false;
            new_state.heuristic = d16_calc_heuristic(
                    new_state,
                    *scratch_rooms,
                    valveroom_to_mask,
                    graph,
                    part1);

            key : d16_full_key;
            key.cur_room = new_state.cur_room;
            key.time_left = new_state.time_left;
            key.first_pawn = new_state.first_pawn;

            default_value : [..]d16_search_state;
            prev_states : *[..]d16_search_state = find_or_add(*full_list, key, default_value);

            // store new state
            heap_push(*open_list, new_state, open_list_cmp);
            array_add(prev_states, new_state);
        }
    }

    // find most pressure in visited
    num_states := 0;
    answer := 0;
    best_state : *d16_search_state;
    for *prev_states : full_list {
        for *prev_state : <<prev_states {
            num_states += 1;
            if prev_state.released_pressure > answer {
                answer = prev_state.released_pressure;
                best_state = prev_state;
            }
        }
     } 

    print("(debug) valve rooms mask: %\n", valverooms_mask);
    print("(debug) best state: %\n", <<best_state);
    print("(debug) num states searched: %\n", num_states);
    //print("(debug) moves: \n%", best_state.debug_moves);
    //print("(debug) path: %\n", best_state.solution_path);
    print("(debug) day16 partX: %\n\n\n", answer);
    return answer;
}

d16_calc_heuristic :: (
    state: d16_search_state,
    scratch_rooms: *[..]*d16_room,
    valveroom_to_mask: Table(RoomIndex, u64),
    graph: []d16_room,
    is_part1: bool
) -> int {
    room_cmp :: (a: *d16_room, b: *d16_room) -> bool {
        return a.rate > b.rate;
    }

    heuristic := state.released_pressure;

    // find unopened rooms
    scratch_rooms.count = 0;
    for valveroom_to_mask {
        room_idx : RoomIndex = it_index;
        mask : u64 = it;

        if (state.opened_valves & mask) == 0 {
            room : *d16_room = *graph[room_idx];
            heap_push(scratch_rooms, room, room_cmp);
        }
    }

    if scratch_rooms.count == 0 return heuristic;

    if is_part1 || (!is_part1 && !state.first_pawn) {
        // minus 2 because one to move, one to open
        time_left := state.time_left - 2;
        while scratch_rooms.count > 0 && time_left > 0 {
            room : *d16_room = heap_pop(scratch_rooms, room_cmp);
            heuristic += time_left * room.rate;
            time_left -= 2;
        }
    } else {
        times_left: [2]int;
        times_left[0] = state.time_left - 2;
        times_left[1] = ifx state.first_pawn then 26 - 2 else 0;

        while scratch_rooms.count > 0 {
            time_idx := ifx times_left[0] > times_left[1] then 0 else 1;
            most_time : *int = *times_left[time_idx];

            if <<most_time <= 2 break;

            room : *d16_room = heap_pop(scratch_rooms, room_cmp);
            heuristic += <<most_time * room.rate;
        
            // minus 2 because one to move, one to open
            most_time -= 2;
        }
    }

    return heuristic;
}

RoomIndex :: #type,distinct int;

d16_room :: struct {
    name : string;
    mask : u64;
    rate : int;
    tunnels : [..]RoomIndex;
    paths : Table(RoomIndex, int);
}

d16_search_state :: struct {
    opened_valves : u64;
    released_pressure : int;
    heuristic : int;
    cur_room : RoomIndex = -1;
    time_left : int = 30;
    first_pawn : bool = true;
}

operator == :: (a: d16_search_state, b: d16_search_state) -> bool {
    return a.opened_valves == b.opened_valves
        && a.released_pressure == b.released_pressure
        && a.heuristic == b.heuristic
        && a.cur_room == b.cur_room
        && a.time_left == b.time_left
        && a.first_pawn == b.first_pawn;
}

get_hash_d16_search_state :: (v: d16_search_state) -> u32 {
    Hash :: #import "Hash";
    result : u32 = 0;
    result = hash_combine(result, Hash.get_hash(v.opened_valves));
    result = hash_combine(result, Hash.get_hash(v.released_pressure));
    result = hash_combine(result, Hash.get_hash(v.heuristic));
    result = hash_combine(result, Hash.get_hash(v.cur_room));
    result = hash_combine(result, Hash.get_hash(v.time_left));
    result = hash_combine(result, Hash.get_hash(v.first_pawn));
    return result;
}

d16_full_key :: struct {
    cur_room: RoomIndex;
    time_left: int;
    first_pawn : bool;
}

operator == :: (a: d16_full_key, b: d16_full_key) -> bool {
    return a.cur_room == b.cur_room
        && a.time_left == b.time_left
        && a.first_pawn == b.first_pawn;
}

get_hash_d16_full_key :: (v : d16_full_key) -> u32 {
    Hash :: #import "Hash";
    return hash_combine(
        hash_combine(
            Hash.get_hash(v.cur_room),
            Hash.get_hash(v.time_left)),
        Hash.get_hash(v.first_pawn));
}


// --------------------------------------------------------
// Day 17
// --------------------------------------------------------
day17 :: () {
    day17_test();
    day17_actual();
}

day17_test :: () {
    input, success := read_entire_file("data/day17_example.txt");
    assert(success);

    assert(day17_solve(input, 2022) == 3068);
    assert(day17_solve(input, 1_000_000_000_000) == 1514285714288);
}

day17_actual :: () {
    input, success := read_entire_file("data/day17.txt");
    assert(success);

    part1 := day17_solve(input, 2022);
    print("day17 part1: %\n", part1);
    assert(part1 == 3048);

    part2 := day17_solve(input, 1_000_000_000_000);
    print("day17 part2: %\n", part2);
    assert(part2 == 1504093567249);
}

day17_solve :: (input: string, iterations: s64) -> s64 {
    board : Table(Vec2, int, get_hash);
    cycles : Table(d17_cycle_key, d17_cycle_state, get_hash_d17_cycle_key);

    piece := 0;
    move := 0;
    top := 0;

    cur_pos : [..]Vec2;
    maybe_pos : [..]Vec2;

    overlaps :: (board: Table(Vec2, int, get_hash), piece: []Vec2) -> bool {
        for tile: piece {
            if tile.x < 0 || tile.x > 6 || tile.y <= 0 return true;

            _, found := table_find(*board, tile);
            if found return true;
        }
        return false;
    }

    draw_board :: (board: Table(Vec2, int, get_hash), piece: int, top: int) {
        print("\n\n\nAfter Piece: %\n", piece);
        
        row := top + 1;
        while row > 0 {
            defer row -= 1;

            print("|");
            for 0..6 {
                ifx table_contains(*board, Vec2.{it, row}) print("#")
                else print(" ");
            }
            print("|\n");
        }
        print("+-------+\n");
    }

    cycle_height := 0;
    found_cycle := false;

    while piece < iterations {
        defer piece += 1;

        // look for cycle
        if !found_cycle {
            // build cycle key
            key : d17_cycle_key;
            key.jet = move % input.count;
            key.piece_type = cast(u8)(piece % 5);
            for col: 0..6 {
                row := top;
                while row > 0 && !table_contains(*board, Vec2.{col, row}) {
                    row -= 1;
                }
                key.depth[col] = cast(u8)(top - row);
            }

            // check for cycle
            prev, found := table_find(*cycles, key);
            if found {
                // compute cycle stats
                pieces_per_cycle := piece - prev.piece;
                pieces_left := iterations - piece;
                num_cycles_left := pieces_left / pieces_per_cycle;
                height_per_cycle := top - prev.top;
                cycle_height = num_cycles_left * height_per_cycle;
                
                // reduce iterations to play it out
                piece += num_cycles_left * pieces_per_cycle;
                assert(piece < iterations);
                found_cycle = true;
            }

            table_set(*cycles, key, d17_cycle_state.{piece, top});
        }
    
        // reset
        cur_pos.count = 0;

        // initialize piece
        spawn_bot := top + 4;
        spawn_left := 2;
        piece_type := piece % 5;
        if piece_type == 0 {
            // shape: -
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left + 1, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left + 2, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left + 3, spawn_bot});
        } else if piece_type == 1 {
            // shape: +
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot + 1});
            array_add(*cur_pos, Vec2.{spawn_left + 1, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left + 1, spawn_bot + 1});
            array_add(*cur_pos, Vec2.{spawn_left + 1, spawn_bot + 2});
            array_add(*cur_pos, Vec2.{spawn_left + 2, spawn_bot + 1});
        } else if piece_type == 2 {
            // shape: _|
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left+1, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left+2, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left+2, spawn_bot+1});
            array_add(*cur_pos, Vec2.{spawn_left+2, spawn_bot+2});
        } else if piece_type == 3 {
            // shape: |
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot+1});
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot+2});
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot+3});
        } else if piece_type == 4 {
            // shape: box
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left + 1, spawn_bot});
            array_add(*cur_pos, Vec2.{spawn_left, spawn_bot + 1});
            array_add(*cur_pos, Vec2.{spawn_left + 1, spawn_bot + 1});
        } else assert(false);     

        // fall until collision
        while true {
            defer move += 1;

            // Calc jet direction
            jet := input.data[move % input.count];
            lateral_offset : Vec2 = ifx jet == #char "<" then Vec2.{-1,0} else Vec2.{1,0};

            // maybe move left/right
            maybe_pos.count = 0;
            for cur_pos array_add(*maybe_pos, add(it, lateral_offset));
            if !overlaps(board, maybe_pos) {
                for maybe_pos cur_pos[it_index] = it;
            }

            // maybe move down
            maybe_pos.count = 0;
            for cur_pos array_add(*maybe_pos, add(it, Vec2.{0, -1}));
            if overlaps(board, maybe_pos) {
                // hit rocks
                for cur_pos {
                    table_set(*board, it, 0);
                    top = max(top, it.y);
                }
                //draw_board(*board, piece, top);
                break;
            } else {
                for maybe_pos cur_pos[it_index] = it;
            }
        }     
    }

    return top + cycle_height;
}

d17_cycle_key :: struct {
    depth : [7]u8;
    piece_type: u8;
    jet : int;
}

d17_cycle_state :: struct {
    piece: int;
    top: int;
}

operator == :: (a: d17_cycle_key, b: d17_cycle_key) -> bool {
    return a.depth[0] == b.depth[0]
        && a.depth[1] == b.depth[1]
        && a.depth[2] == b.depth[2]
        && a.depth[3] == b.depth[3]
        && a.depth[4] == b.depth[4]
        && a.depth[5] == b.depth[5]
        && a.depth[6] == b.depth[6]
        && a.piece_type == b.piece_type
        && a.jet == b.jet;
}

get_hash_d17_cycle_key :: (v : d17_cycle_key) -> u32 {
    Hash :: #import "Hash";
    result : u32 = 0;
    for v.depth result = hash_combine(result, Hash.get_hash(it));
    result = hash_combine(result, Hash.get_hash(v.piece_type));
    result = hash_combine(result, Hash.get_hash(v.jet));
    return result;
}

// --------------------------------------------------------
// Day 18
// --------------------------------------------------------
day18 :: () {
    day18_test();
    day18_actual();
}

day18_test :: () {
    input, success := read_entire_file("data/day18_example.txt");
    assert(success);

    part1, part2 := day18_solve(input);
    assert(part1 == 64);
    assert(part2 == 58);
}

day18_actual :: () {
    input, success := read_entire_file("data/day18.txt");
    assert(success);

    part1, part2 := day18_solve(input);
    print("day18 part1: %\n", part1);
    print("day18 part2: %\n", part2);
    assert(part1 == 3586);
    assert(part2 == 2072);
}

day18_solve :: (input: string) -> int, int {
    shape_min := Vec3.{10000, 10000, 10000};
    shape_max := Vec3.{-10000, -10000, -10000};

    // parse shape
    shape : Table(Vec3, int, get_hash_vec3);
    for line : split(input, "\r\n") {
        parts := split(line, ",");
        assert(parts.count == 3);

        pos := Vec3.{
            string_to_int(parts[0]),
            string_to_int(parts[1]),
            string_to_int(parts[2]),
        };

        shape_min.x = min(shape_min.x, pos.x);
        shape_min.y = min(shape_min.y, pos.y);
        shape_min.z = min(shape_min.z, pos.z);

        shape_max.x = max(shape_max.x, pos.x);
        shape_max.y = max(shape_max.y, pos.y);
        shape_max.z = max(shape_max.z, pos.z);

        table_add(*shape, pos, 0);
    }

    // part1
    offsets : [6]Vec3;
    offsets[0] = Vec3.{-1,0,0};
    offsets[1] = Vec3.{1,0,0};
    offsets[2] = Vec3.{0,-1,0};
    offsets[3] = Vec3.{0,1,0};
    offsets[4] = Vec3.{0,0,-1};
    offsets[5] = Vec3.{0,0,1};

    part1 := 0;
    for shape {
        pos := it_index;
        for offset: offsets {
            neighbor := pos + offset;
            if !table_contains(*shape, neighbor) {
                part1 += 1;
            }
        }
    }


    // part2
    part2 := 0;

    // expand shape
    shape_min += Vec3.{-1,-1,-1};
    shape_max += Vec3.{1,1,1};

    visited : Table(Vec3, int, get_hash_vec3);
    open_list : [..]Vec3;
    array_add(*open_list, shape_min);
    table_set(*visited, shape_min, 0);

    while open_list.count > 0 {
        pos := pop(*open_list);

        for offset : offsets {
            neighbor := pos + offset;

            if neighbor.x < shape_min.x || neighbor.x > shape_max.x
                || neighbor.y < shape_min.y || neighbor.y > shape_max.y
                || neighbor.z < shape_min.z || neighbor.z > shape_max.z 
            {
                continue;
            }

            if table_contains(*shape, neighbor) {
                part2 += 1;
            }
            else if !table_contains(*visited, neighbor) {
                array_add(*open_list, neighbor);
                table_set(*visited, neighbor, 0);
            }
        }
    }

    return part1, part2;
}

day18_part2 :: (input: string) -> int {

    // parse shape
    shape : Table(Vec3, int, get_hash_vec3);
    for line : split(input, "\r\n") {
        parts := split(line, ",");
        assert(parts.count == 3);

        pos := Vec3.{
            string_to_int(parts[0]),
            string_to_int(parts[1]),
            string_to_int(parts[2]),
        };

        table_add(*shape, pos, 0);
    }

    return 0;
}

// --------------------------------------------------------
// Day 19
// --------------------------------------------------------
day19 :: () {
    day19_test();
    day19_actual();
}

day19_test :: () {
    input, success := read_entire_file("data/day19_example.txt");
    assert(success);

    part1 := day19_solve(input, true);
    print("(test) day19 part1: %\n", part1);
    assert(part1 == 33);

    part2 := day19_solve(input, false);
    print("(test) day19 part2: %\n", part2);
    assert(part2 == 3472);
}

day19_actual :: () {
    input, success := read_entire_file("data/day19.txt");
    assert(success);

    part1 := day19_solve(input, true);
    print("day19 part1: %\n", part1);
    assert(part1 == 1653);

    part2 := day19_solve(input, false);
    print("day19 part2: %\n", part2);
    assert(part2 == 4212);
}

day19_solve :: (input: string, is_part1: bool) -> int {
    // minutes depends on part
    num_minutes := ifx is_part1 then 24 else 32;

    // parse blueprints
    blueprints : [..]d19_blueprint;
    for line : split(input, "\r\n") {
        parts := split(line, " ");
        assert(parts.count == 32);

        // parse blueprint
        bp : d19_blueprint;
        bp.id = string_to_int(parts[1]);
        bp.ore_robot_ore_cost = string_to_int(parts[6]);
        bp.clay_robot_ore_cost = string_to_int(parts[12]);
        bp.obsidian_robot_ore_cost = string_to_int(parts[18]);
        bp.obsidian_robot_clay_cost = string_to_int(parts[21]);
        bp.geode_robot_ore_cost = string_to_int(parts[27]);
        bp.geode_robot_obsidian_cost = string_to_int(parts[30]);

        array_add(*blueprints, bp);
    }

    // only top 3 blueprints for part 2
    if !is_part1 {
        blueprints.count = min(blueprints.count, 3);
    }

    answer := ifx is_part1 then 0 else 1;
    best_state : d19_state;
    for bp : blueprints {
        best_geodes := 0;

        // don't bother building more ore robots than maximum ore cost
        max_ore_robots := max(bp.ore_robot_ore_cost, bp.clay_robot_ore_cost);
        max_ore_robots = max(max_ore_robots, bp.obsidian_robot_ore_cost);
        max_ore_robots = max(max_ore_robots, bp.geode_robot_ore_cost);

        // prepare search
        visited : Table(d19_state, int, get_hash_d19_state);
        open_list : [..]d19_state;

        // initialize with both ORE and CLAY robots as initial goal
        array_add(*open_list, d19_state.{goal=0});
        array_add(*open_list, d19_state.{goal=1});

        // exhaustive search
        while open_list.count > 0 {
            cur_state := pop(*open_list);

            new_state := cur_state;

            // add resources
            new_state.ore += new_state.ore_robots;
            new_state.clay += new_state.clay_robots;
            new_state.obsidian += new_state.obsidian_robots;
            new_state.geodes += new_state.geode_robots;

            // end of the line, no more time
            if new_state.minute == num_minutes {
                if new_state.geodes > best_geodes {
                    best_geodes = new_state.geodes;
                    best_state = new_state;
                }
                continue;
            }

            // increment time
            new_state.minute += 1;


            // theoretical best geodes
            time_left := num_minutes - new_state.minute + 1;
            best_case := new_state.geodes 
                + (new_state.geode_robots * (time_left))
                + ((time_left*(time_left + 1))/2);

            // just stop if the best possible scenario for this state isn't good enough
            if best_case < best_geodes continue;


            // create an ore robot
            if cur_state.goal == 0 && cur_state.ore_robots <= max_ore_robots
                && cur_state.ore >= bp.ore_robot_ore_cost
            {
                new_state.ore -= bp.ore_robot_ore_cost;
                new_state.ore_robots += 1;
                new_state.goal = -1;
            }

            // create a clay robot
            if cur_state.goal == 1 && cur_state.ore >= bp.clay_robot_ore_cost {
                new_state.ore -= bp.clay_robot_ore_cost;
                new_state.clay_robots += 1;
                new_state.goal = -1;
            }

            // create an obsidian robot
            if cur_state.goal == 2 && cur_state.ore >= bp.obsidian_robot_ore_cost
                && cur_state.clay >= bp.obsidian_robot_clay_cost
            {
                new_state.ore -= bp.obsidian_robot_ore_cost;
                new_state.clay -= bp.obsidian_robot_clay_cost;
                new_state.obsidian_robots += 1;
                new_state.goal = -1;
            }

            // create a geode robot
            if cur_state.goal == 3 && cur_state.ore >= bp.geode_robot_ore_cost
                    && cur_state.obsidian >= bp.geode_robot_obsidian_cost
            {
                new_state.ore -= bp.geode_robot_ore_cost;
                new_state.obsidian -= bp.geode_robot_obsidian_cost;
                new_state.geode_robots += 1;
                new_state.goal = -1;
            }

            if new_state.goal != -1 {
                // did not build a robot. append state and keep going.
                if !table_contains(*visited, new_state) {
                    array_add(*open_list, new_state);
                    table_set(*visited, new_state, 0);
                }
            }
            else {
                // built a robot! consider all possible robots as next goals

                // try to build an ore robot
                if new_state.ore_robots < max_ore_robots {
                    // make our goal an ore robot
                    s := new_state;
                    s.goal = 0;

                    if !table_contains(*visited, s) {
                        array_add(*open_list, s);
                        table_set(*visited, s, 0);
                    }
                }

                // try to build a clay robot
                {
                    s := new_state;
                    s.goal = 1;

                    if !table_contains(*visited, s) {
                        array_add(*open_list, s);
                        table_set(*visited, s, 0);
                    }
                }

                // try to build an obsidian robot
                if new_state.clay_robots > 0 {
                    s := new_state;
                    s.goal = 2;

                    if !table_contains(*visited, s) {
                        array_add(*open_list, s);
                        table_set(*visited, s, 0);
                    }
                }

                // try to build a geode robot
                if new_state.obsidian_robots > 0 {
                    s := new_state;
                    s.goal = 3;

                    if !table_contains(*visited, s) {
                        array_add(*open_list, s);
                        table_set(*visited, s, 0);
                    }
                }
            }
        }

        if is_part1 answer += best_geodes * bp.id;
        else answer *= best_geodes;
    }

    return answer;
}

d19_blueprint :: struct {
    id : int;
    ore_robot_ore_cost : int;
    clay_robot_ore_cost: int;
    obsidian_robot_ore_cost: int;
    obsidian_robot_clay_cost: int;
    geode_robot_ore_cost : int;
    geode_robot_obsidian_cost: int;
}

d19_state :: struct {
    minute: int = 1;

    goal: int = -1;

    ore: int;
    clay: int;
    obsidian: int;
    geodes: int;

    ore_robots: int = 1;
    clay_robots: int;
    obsidian_robots: int;
    geode_robots: int;
}

get_hash_d19_state :: (v: d19_state) -> u32 {
    Hash :: #import "Hash";
    result : u32 = 0;
    result = hash_combine(result, Hash.get_hash(v.minute));
    result = hash_combine(result, Hash.get_hash(v.goal));

    result = hash_combine(result, Hash.get_hash(v.ore));
    result = hash_combine(result, Hash.get_hash(v.clay));
    result = hash_combine(result, Hash.get_hash(v.obsidian));
    result = hash_combine(result, Hash.get_hash(v.geodes));

    result = hash_combine(result, Hash.get_hash(v.ore_robots));
    result = hash_combine(result, Hash.get_hash(v.clay_robots));
    result = hash_combine(result, Hash.get_hash(v.obsidian_robots));
    result = hash_combine(result, Hash.get_hash(v.geode_robots));

    return result;
}

operator == :: (a: d19_state, b: d19_state) -> bool {
    return a.minute == b.minute
        && a.ore == b.ore
        && a.clay == b.clay
        && a.obsidian == b.obsidian
        && a.geodes == b.geodes
        && a.ore_robots == b.ore_robots
        && a.clay_robots == b.clay_robots
        && a.obsidian_robots == b.obsidian_robots
        && a.geode_robots == b.geode_robots;
}

// --------------------------------------------------------
// Day 20
// --------------------------------------------------------
day20 :: () {
    day20_test();
    //day20_actual();
}

day20_test :: () {
    input, success := read_entire_file("data/day20_example.txt");
    assert(success);

    part1 := day20_solve(input, true);
    print("(test) day20 part1: %\n", part1);
    assert(part1 == 3);

    part2 := day20_solve(input, false);
    print("(test) day20 part2: %\n", part2);
    assert(part2 == 1623178306);

    // part2 := day20_solve(input, false);
    // print("(test) day20 part2: %\n", part2);
    // assert(part2 == 3472);
}

day20_actual :: () {
    input, success := read_entire_file("data/day20.txt");
    assert(success);

    part1 := day20_solve(input, true);
    print("day20 part1: %\n", part1);
    assert(part1 == 6387);

    part2 := day20_solve(input, false);
    print("day20 part2: %\n", part2);
}

day20_solve :: (input: string, is_part1: bool) -> s64 {
    zero_idx := 0;
    decryption_key :: 811589153;

    // parse input
    nums : [..]d20_node;
    lines := split(input, "\r\n");
    for line : lines {
        prev := it_index - 1;
        next := it_index + 1;
        value := string_to_int(line);
        if prev == -1 prev = lines.count - 1;
        if next == lines.count next = 0;

        if value == 0 zero_idx = nums.count;

        if !is_part1 value *= decryption_key;

        array_add(*nums, d20_node.{value, prev, next});
    }

    // mix the encrypted file
    // print("Unmixed: ");
    // for nums print("%  ", it.value);
    // print("\n");

    num_mixes := ifx is_part1 then 1 else 10;

    for 0..num_mixes - 1 {
        for idx: 0..nums.count - 1 {
            // 0 doesn't move
            if idx == zero_idx continue;

            init_prev := nums[idx].prev;
            init_next := nums[idx].next;
            init_value := nums[idx].value;

            //print( "    Moving %", init_value);

            // remove from initial spot
            nums[init_prev].next = init_next;
            nums[init_next].prev = init_prev;

            lo_idx := -1;
            hi_idx := -1;

            if init_value > 0 {
                // follower "pointers" forward
                insert_idx := idx;
                steps := init_value;

                // minus one because we removed this element
                if steps > 2*nums.count steps = steps % (nums.count - 1);
                //else steps = -((Math.abs(steps) % nums.count));

                for 0..steps - 1 {
                    insert_idx = nums[insert_idx].next;
                }

                lo_idx = insert_idx;
                hi_idx = nums[insert_idx].next;
            } else {
                // follow "pointers" backwards
                insert_idx := idx;
                for 0..Math.abs(init_value) -1 {
                    insert_idx = nums[insert_idx].prev;
                }

                hi_idx = insert_idx;
                lo_idx = nums[insert_idx].prev;
            }
            assert(lo_idx != -1);
            assert(hi_idx != -1);

            // insert between
            //print("   Insert between % and %:   ", nums[lo_idx].value, nums[hi_idx].value);
            nums[idx].prev = lo_idx;
            nums[idx].next = hi_idx;
            nums[lo_idx].next = idx;
            nums[hi_idx].prev = idx;


            // i := zero_idx;
            // for 0..count {
            //     print("% ", nums[i].value);
            //     i = nums[i].next;
            // }
            // print("\n");
        }
    }

    // print("Mixed: ");
    // idx := zero_idx;
    // for 0..count {
    //     print("% ", nums[idx].value);
    //     idx = nums[idx].next;
    // }
    // print("\n");


    all_steps : [3]int;
    all_steps[0] = 1000;
    all_steps[1] = 2000;
    all_steps[2] = 3000;

    part1 := 0;
    for steps : all_steps {
        actual_steps := steps % nums.count;

        idx := zero_idx;
        for 0..actual_steps-1 {
            idx = nums[idx].next;
        }
        //print("% (%) after 0 is: %\n", steps, actual_steps, nums[idx].value);
        part1 += nums[idx].value;
    }

    return part1;
}

d20_node :: struct {
    value: s64;
    prev: int;
    next: int;
}

#import "Basic";
#import "Compiler";
#import "Deep_Copy";
#import "File";

Hash :: #import "Hash";
#poke_name Hash get_hash;

#import "Hash_Table";
Hash_Table :: #import "Hash_Table";
#poke_name Hash_Table operator==;

Math :: #import "Math";
#import "String";
#import "Sort";
#import "Bit_Operations";
