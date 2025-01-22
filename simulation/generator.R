# Function to simulate the BKT model data
simulate_bkt_data <- function(prior, guess, slip, learn, num_students, max_questions, output_file = "simulation_data.csv") {
  
  # Function to simulate a single student's answers using BKT model
  simulate_student <- function(student_id, prior, guess, slip, learn, max_questions) {
    
    # Initialize student skill state (true or false)
    skill_state <- prior
    # Initialize a vector to store the student's answers
    answers <- numeric(max_questions)
    skill_name <- "mathematic"  # This is a string
    
    # Create a data frame to store the results
    student_data <- data.frame(
      order_id = integer(max_questions),  # Integer column
      correct = integer(max_questions),   # Integer column
      student_id = integer(max_questions),  # Integer column
      skill_name = character(max_questions)  # Character column
    )
    
    for (i in 1:max_questions) {
      # Determine if the student gets the question correct based on the skill state
      p_correct <- ifelse(skill_state, 1 - slip, guess)
      correct <- rbinom(1, 1, p_correct)
      
      # Update the skill state based on whether the answer was correct
      if (correct == 1) {
        skill_state <- min(skill_state + learn, 1)
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
  write.csv(final_data, output_file, row.names = FALSE)
  
  # Return the final data frame
  return(final_data)
}

# Example usage:
# Define parameters for the BKT model
prior <- 0.2
guess <- 0.3
slip <- 0.2
learn <- 0.1
num_students <- 50
max_questions <- 10

# Call the function to simulate the data and save it to a CSV
simulate_bkt_data(prior, guess, slip, learn, num_students, max_questions)
