
# MARK: convert_data
convert_data <- function(data_path, skill_name, defaults = defaults, model_type = model_type, 
                           gs_refs = gs_ref, resource_refs = resource_ref, return_df = return_df, folds = folds) {
  if(is.null(model_type)) {
    multilearn = multiprior = multipair = multigs = False
  } else {
    multilearn = model_type[1]
    multiprior = model_type[2]
    multipair = model_type[3]
    multigs = model_type[4]
  }
  df = read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(df)) {
    stop("Failed to load the CSV file.")
  }
  # default column names for cognitive tutors
    ct_default <- list(
        order_id = "Row",
        skill_name = "KC(Default)",
        correct = "Correct First Attempt",
        user_id = "Anon Student Id",
        multilearn = "Problem Name",
        multiprior = "Correct First Attempt",
        multipair = "Problem Name",
        multigs = "Problem Name",
        folds = "Anon Student Id"
    )
    # integrate custom defaults with default assistments/ct columns if they are still unspecified
    if(is.null(defaults)) {
        defaults = list()
    } else {
        stop("not implemented")
    }

    df_columns <- colnames(df)
    # 遍历 ct_default 列表的键值对
    for (key in names(ct_default)) {
        value <- ct_default[[key]]
        if (key %in% df_columns) {
            defaults[[key]] <- key
        } else if (value %in% df_columns) {
            defaults[[key]] <- value
        }
    }
    # change df columns names
    df_columns <- colnames(df)
    for (key in names(defaults)) {
        value <- defaults[[key]]
        df_columns[df_columns == value] <- key
    }
    colnames(df) <- df_columns

    # sort df by order_id
    df$order_id <- as.numeric(df$order_id)
    df <- df[order(df$order_id), ]

    # 检查是否存在 "user_id", "correct", 和 "skill_name" 在 defaults 中
    required_columns <- c("user_id", "correct", "skill_name")
    for (col in required_columns) {
        if (!(col %in% names(df))) {
            stop(paste("The required column (", col, ") is missing in the dataframe."))
        }
    }
    # order by user_id
    df$user_id <- as.character(df$user_id)
    df <- df[order(ascii_order(df$user_id), df$order_id), ]
    if ("original" %in% colnames(df)) {
        df <- df[df$original == 1, ]
    }
    df[["skill_name"]] <- as.character(df[["skill_name"]])

    tryCatch({
        df[["correct"]] <- as.integer(df[["correct"]])
    }, warning = function(w) {
        stop("Invalid Data In Specified Corrects Column")
    }, error = function(e) {
        stop("Invalid Data In Specified Corrects Column")
    })

    # handle skills
    datas = list()
    skill_name <- paste0("^(", skill_name, ")$")
    all_skills <- unique(na.omit(df[["skill_name"]]))
    all_skills <- as.character(all_skills)
    all_skills <- all_skills[grepl(skill_name, all_skills)]
    if (length(all_skills) == 0) {
        stop("No matching skills")
    }
    for (skill_ in all_skills) {
    
        if (is.null(resource_refs) || !(skill_ %in% names(resource_refs))) {
            resource_ref <- NULL
        } else {
            resource_ref <- resource_refs[[skill_]][["resource_names"]]
        }
        if (is.null(gs_refs) || !(skill_ %in% names(gs_refs))) {
            gs_ref <- NULL
        } else {
            gs_ref <- gs_refs[[skill_]][["gs_names"]]
        }
        df3 <- df[df[["skill_name"]] == skill_, ]

        if (nrow(df3) == 0) {
            stop("Incorrect Skill or Dataset Specified")
        }
        stored_index <- rownames(df3)
        multiprior_index <- NULL
        correct_values <- unique(df3[["correct"]])
        if (!all(correct_values %in% c(-1, 0, 1))) {
            stop("Correctness must be -1 (no response), 0 (incorrect), or 1 (correct)")
        }
        df3[["correct"]] <- df3[["correct"]] + 1
        data <- as.matrix(df3[["correct"]])
        Data <- list()
        user_ids <- df3[["user_id"]]
        lengths <- as.integer(table(factor(user_ids, levels = unique(user_ids))))

        starts <- integer(length(lengths))
        starts[1] <- 1
        for (i in 2:length(lengths)) {
            starts[i] <- starts[i - 1] + lengths[i - 1]
        }

        if (multipair + multiprior + multilearn > 1) {
            stop("cannot specify more than 1 resource handling")
        }

        if (multipair) {
            stop("not implemented")
        } else if(multiprior) {
            stop("not implemented")
        } else if(multilearn) {
            stop("not implemented")
        } else {
            resources <- rep(1, length(data))
        }
        
        if(multigs) {
            stop("not implemented")
        } else {
            data <- list(data)
            Data <- list("data" = as.integer(unlist(data)))
        }

        if (!multilearn && !multipair && !multiprior) {
            resource_ref <- list()
            resource_ref[["default"]] <- 1
        }
        if (!multigs) {
            gs_ref <- list()
            gs_ref[["default"]] <- 1
        }
        Data$starts = starts
        Data$lengths = lengths
        Data$resources = resources
        Data$resource_names = resource_ref
        Data$gs_names = gs_ref
        Data$index = stored_index
        Data$multiprior_index = multiprior_index

        if(folds) {
            Data[["folds"]] <- as.integer(df3[["folds"]])
        }
        datas[[skill_]] <- Data
    }
    return(datas)
}

# const require to sort string by ascii
ascii_order <- function(x) {
  sapply(seq_along(x), function(i) {
    paste0(sprintf("%03d", utf8ToInt(substr(x[i], 1, nchar(x[i])))), collapse = "")
  })
}
