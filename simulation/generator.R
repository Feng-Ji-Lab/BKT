# Function to simulate the BKT model data
simulate_bkt_data <- function(prior, guess, slip, learn, num_students, max_questions, output_file = NULL) {
  # Function to simulate a single student's answers using BKT model
  simulate_student <- function(student_id, prior, guess, slip, learn, max_questions) {
    # print(student_id)
    # Initialize student skill state (true or false)
    skill_state <- prior
    skill_name <- "mathematic" # This is a string

    # Create a data frame to store the results
    student_data <- data.frame(
      order_id = integer(max_questions), # Integer column
      correct = integer(max_questions), # Integer column
      student_id = integer(max_questions), # Integer column
      skill_name = character(max_questions) # Character column
    )
    # Determine if the student gets the question correct based on the skill state
    know_rate <- rbinom(1, 1, skill_state)

    for (i in 1:max_questions) {
      # Probability of correct
      p_correct <- know_rate * (1 - slip) + (1 - know_rate) * guess

      correct <- rbinom(1, 1, p_correct)
      # print(c(know_rate, p_correct, correct))

      # Update the skill state based on whether the answer was correct
      if (know_rate == 0) {
        know_rate <- rbinom(1, 1, learn)
      }

      # Add the row for the current question
      student_data[i, ] <- c(i, correct, student_id, skill_name)
    }

    return(student_data)
  }

  # Initialize a list to store all students' data
  all_student_data <- list()

  # Simulate for each student
  for (student_id in 1:num_students) {
    student_data <- simulate_student(student_id, prior, guess, slip, learn, sample(1:max_questions, 1)) # Randomly select number of questions
    all_student_data[[student_id]] <- student_data
  }

  # Combine all students' data into one data frame
  final_data <- do.call(rbind, all_student_data)

  # Ensure that the first three columns are numeric
  final_data$order_id <- as.numeric(final_data$order_id)
  final_data$correct <- as.numeric(final_data$correct)
  final_data$student_id <- as.numeric(final_data$student_id)

  # Write the data to CSV
  if (!is.null(output_file)) {
    write.csv(final_data, output_file, row.names = FALSE)
  }

  # Return the final data frame
  return(final_data)
}

# Example usage:
prior <- 0.2
guess <- 0.1
slip <- 0.1
learn <- 0.3
num_students <- 2000
max_questions <- 10
simulate_bkt_data(prior, guess, slip, learn, num_students, max_questions, output_file = "simulation_data_2000.csv")
