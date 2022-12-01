main :: () {
    day_01();
}

day_01 :: () {
    day_01_test();
}

day_01_part1 :: () {
}

day_01_test :: () {
    filename := "data/day01_example.txt";

    file : string;
    success: bool;
    file, success = read_entire_file(filename);
    assert(success);

    print(file);
}

#import "Basic";
#import "File";