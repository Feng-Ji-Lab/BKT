#' Simulate Data from a Bayesian Knowledge Tracing (BKT) Model
#'
#' This function generates simulated student response data based on the BKT model.
#' Each student is modeled as having a latent skill mastery state that can evolve over a sequence of
#' activities according to specified learning, guessing, and slipping probabilities.
#' The generated dataset can be used for testing, model evaluation, or demonstration purposes.
#'
#' @param prior Numeric. Initial probability that a student has mastered the skill before any questions.
#'   Corresponds to the BKT parameter \(P_p\).
#' @param guess Numeric. Probability that a student answers correctly despite not mastering the skill.
#'   Corresponds to the BKT parameter \(P_g\).
#' @param slip Numeric. Probability that a student answers incorrectly despite mastering the skill.
#'   Corresponds to the BKT parameter \(P_s\).
#' @param learn Numeric. Probability that a student transitions from the unmastered to mastered state after each activity.
#'   Corresponds to the BKT parameter \(P_l\).
#' @param num_students Integer. Number of students to simulate.
#' @param max_questions Integer. Maximum number of questions (activities) per student.
#'   Each student's actual sequence length is randomly drawn from 1 to \code{max_questions}.
#' @param output_file Character. Optional file path. If provided, the generated dataset will be written to a CSV file.
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item \code{order_id}: Integer, the order of the question within a student's sequence.
#'     \item \code{correct}: Binary (0/1), indicating whether the student answered correctly.
#'     \item \code{student_id}: Integer, the unique identifier for each student.
#'     \item \code{skill_name}: Character, the skill associated with the responses (currently fixed as "mathematic").
#'   }
#'
#' @details
#' The simulated responses follow the standard BKT transition logic:
#' \deqn{P(K_{t+1} = 1) = P(K_t = 1) + (1 - P(K_t = 1)) \times P_l}
#' and the observed correctness is drawn as:
#' \deqn{P(C_t = 1) = P(K_t = 1)(1 - P_s) + (1 - P(K_t = 1))P_g.}
#'
#' @examples
#' \donttest{
#' prior <- 0.2
#' guess <- 0.1
#' slip <- 0.1
#' learn <- 0.3
#' num_students <- 2000
#' max_questions <- 10
#'
#' # Simulate and save to CSV
#' simulate_bkt_data(
#'     prior = prior,
#'     guess = guess,
#'     slip = slip,
#'     learn = learn,
#'     num_students = num_students,
#'     min_questions = min_questions,
#'     max_questions = max_questions,
#'     output_file = "simulation_data_2000.csv"
#' )
#' }
#'
#' @export
simulate_bkt_data <- function(prior, guess, slip, learn, num_students, min_questions, max_questions, output_file = NULL) {
    simulate_student <- function(student_id, prior, guess, slip, learn, num_questions) {
        skill_name <- "mathematic"

        student_data <- data.frame(
            order_id = integer(num_questions),
            correct = integer(num_questions),
            student_id = integer(num_questions),
            skill_name = character(num_questions)
        )
        know_rate <- rbinom(1, 1, prior) # k1^j ~ Bernoulli(Pp)

        for (i in 1:num_questions) {
            p_correct <- know_rate * (1 - slip) + (1 - know_rate) * guess # Probability of correct

            correct <- rbinom(1, 1, p_correct) # ai^j ∼ Bernoulli(Pcorrect)
            if (know_rate == 0) {
                know_rate <- rbinom(1, 1, learn) # Learning: ki+1^j ~ Bernoulli(Pl) if ki^j = 0
            } # else know_state remains unchaged.

            student_data[i, ] <- c(i, correct, student_id, skill_name)
        }

        return(student_data)
    }

    all_student_data <- list()

    for (student_id in 1:num_students) {
        student_data <- simulate_student(student_id, prior, guess, slip, learn, sample(min_questions:max_questions, 1))
        all_student_data[[student_id]] <- student_data
    }

    final_data <- do.call(rbind, all_student_data)

    final_data$order_id <- as.numeric(final_data$order_id)
    final_data$correct <- as.numeric(final_data$correct)
    final_data$student_id <- as.numeric(final_data$student_id)

    if (!is.null(output_file)) {
        write.csv(final_data, output_file, row.names = FALSE)
    }

    return(final_data)
}