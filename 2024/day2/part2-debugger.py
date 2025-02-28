

with open("test_input.txt", "r") as report_file:
    tolerance_count = 1
    safe_count = 0
    for report in report_file:
        prev = None
        change_type = None
        for level in report:
            prev = level 

            if prev and not change_type:
                pass
                



