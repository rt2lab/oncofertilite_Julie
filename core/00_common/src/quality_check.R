library("stringr")

quality_check_for_variable <- function(row, var_name, data_dict, db_name, family_var) {
    # these variables will be checked
    VARIABLES_LIST <- c("var", "var_type", "levels")

    # TODO: remove NA from var
    expected_variables <- data_dict[data_dict$family_var == family_var,][, VARIABLES_LIST]
    errors = c()

    # TODO: check that all variables exist in data dictionnary (generic or derived)

    # checking variable presence
    if (!(var_name %in% unique(expected_variables$var))){
        errors <- c(errors, sprintf("Variable %s is not in data dict", var_name))
    } else {
        levels = na.omit(expected_variables[factor(expected_variables$var) == 'database',], "var")$levels

        if (length(levels) > 1){
            expected_levels = str_extract_all(levels, regex("([0-9]+):"))
            actual_levels = levels(factor(row))
            levels_are_valid = all(actual_levels %in% expected_levels[[1]])

            if (!levels_are_valid){
                errors <- c(errors, sprintf("non valid levels for %s", row))
            }
        }
    }

    return(errors)
}
