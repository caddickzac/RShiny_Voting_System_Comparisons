
# r shiny
library("shiny")
library("tidyverse")
library('gridExtra')
library('shinydashboard')
library('shinyjs')
library('ggforce')

# resource: 
# https://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r
# https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

ui <- shinyUI(fluidPage(
  headerPanel('Simulated Voting System Outcome Comparisons'),
  sidebarPanel(position='left',
               # numericInput(ID, LABEL TEXT, DEFAULT INPUT, MIN, MAX)
               numericInput('total_voters', 
                            HTML("Number of voters: <font style=font-weight:normal>(max=500)</font>"),
                            30,
                            min = 1, max = 500),
               numericInput('candidate_count',
                            HTML("Number of candidates: <font style=font-weight:normal>(max=20)</font>"),
                            3,
                            min = 1, max = 20),
               selectInput('voting_system', 'See full results:',
                           c('Plurality'='plurality',
                             'Ranked-Choice'='ranked_choice',
                             'Approval'='approval',
                             'Score'='score')
               )
  ),
  mainPanel(
    column(6,plotOutput(outputId="plotgraph", width="800px",height="400px")))
))

server <- shinyServer(function(input, output){
  # Create voter dimensions (x,y)
  Voter_Dim_1 <- reactive({
    rand_dimension(ifelse(input$total_voters>1000,1000,input$total_voters))
  })
  Voter_Dim_2 <- reactive({
    rand_dimension(ifelse(input$total_voters>1000,1000,input$total_voters))
  })
  # Create Voter data
  voterData <- reactive({
    tibble(Voter_Dim_1(),Voter_Dim_2()) # voter_data <-
  })
  # Create candidate dimensions (x,y)
  Candidate_Dim_1 <- reactive({
    # x <- ifelse(input$candidate_count>20,20,input$candidate_count)
    rand_dimension(ifelse(input$candidate_count>20,20,input$candidate_count))
  })
  Candidate_Dim_2 <- reactive({
    rand_dimension(ifelse(input$candidate_count>20,20,input$candidate_count))
  })
  # Name candidates
  Candidate_Names <- reactive({
    as.character(c(seq(1:input$candidate_count)))
  })
  # Create Candidate data
  candidateData <- reactive({
    tibble(Candidate_Dim_1(),Candidate_Dim_2(), Candidate_Names()) # candidate_data
  })
  
  #-----------------------------------------------------------------------------------------
  
  # measure distance a voter is from candidate
  voter_candidate_distance_single_input <- function(candidate_x1, candidate_y1){
    # empty list to be filled with distance formula output
    distance_output <- c()
    # loop for calculating distance for each voter from each candidate
    for(i in 1:input$total_voters){
      distance_output <- append(distance_output,
                                sqrt(((voterData()[i,1,1])-candidate_x1)^2 +
                                       ((voterData()[i,2,1])-candidate_y1)^2),
                                after = length(distance_output))
    }
    return(distance_output)
  }
  
  candidate_distance <- reactive({
    matrix(
      c(
        # candidate 1 
        voter_candidate_distance_single_input(
          candidateData()[1,1,1],
          candidateData()[1,2,1]
        ),
        # candidate 2
        voter_candidate_distance_single_input(
          candidateData()[2,1,1],
          candidateData()[2,2,1]
        ),
        # candidate 3
        voter_candidate_distance_single_input(
          candidateData()[3,1,1],
          candidateData()[3,2,1]
        ),
        # candidate 4
        voter_candidate_distance_single_input(
          candidateData()[4,1,1],
          candidateData()[4,2,1]
        ),
        # candidate 5
        voter_candidate_distance_single_input(
          candidateData()[5,1,1],
          candidateData()[5,2,1]
        ),
        # candidate 6
        voter_candidate_distance_single_input(
          candidateData()[6,1,1],
          candidateData()[6,2,1]
        ),
        # candidate 7
        voter_candidate_distance_single_input(
          candidateData()[7,1,1],
          candidateData()[7,2,1]
        ),
        # candidate 8
        voter_candidate_distance_single_input(
          candidateData()[8,1,1],
          candidateData()[8,2,1]
        ),
        # candidate 9
        voter_candidate_distance_single_input(
          candidateData()[9,1,1],
          candidateData()[9,2,1]
        ),
        # candidate 10
        voter_candidate_distance_single_input(
          candidateData()[10,1,1],
          candidateData()[10,2,1]
        ),
        # candidate 11
        voter_candidate_distance_single_input(
          candidateData()[11,1,1],
          candidateData()[11,2,1]
        ),
        # candidate 12
        voter_candidate_distance_single_input(
          candidateData()[12,1,1],
          candidateData()[12,2,1]
        ),
        # candidate 13
        voter_candidate_distance_single_input(
          candidateData()[13,1,1],
          candidateData()[13,2,1]
        ),
        # candidate 14
        voter_candidate_distance_single_input(
          candidateData()[14,1,1],
          candidateData()[14,2,1]
        ),
        # candidate 15
        voter_candidate_distance_single_input(
          candidateData()[15,1,1],
          candidateData()[15,2,1]
        ),
        # candidate 16
        voter_candidate_distance_single_input(
          candidateData()[16,1,1],
          candidateData()[16,2,1]
        ),
        # candidate 17
        voter_candidate_distance_single_input(
          candidateData()[17,1,1],
          candidateData()[17,2,1]
        ),
        # candidate 18
        voter_candidate_distance_single_input(
          candidateData()[18,1,1],
          candidateData()[18,2,1]
        ),
        # candidate 19
        voter_candidate_distance_single_input(
          candidateData()[19,1,1],
          candidateData()[19,2,1]
        ),
        # candidate 20
        voter_candidate_distance_single_input(
          candidateData()[20,1,1],
          candidateData()[20,2,1]
        )
      ),
      nrow=input$total_voters,# candidate number
      ncol=input$candidate_count)# candidate number
  })
  
  # rank candidates by distance (used for ranked-choice voting)
  preference_ranker_function <- function(voter_data_with_distance){ # dataframe input: combined_data()
    
    # Create Blank Arrays to fill in preference data for (up to) 20 candidates
    Preference_1 <- c()
    Preference_2 <- c()
    Preference_3 <- c()
    Preference_4 <- c()
    Preference_5 <- c()
    Preference_6 <- c()
    Preference_7 <- c()
    Preference_8 <- c()
    Preference_9 <- c()
    Preference_10 <- c()
    Preference_11 <- c()
    Preference_12 <- c()
    Preference_13 <- c()
    Preference_14 <- c()
    Preference_15 <- c()
    Preference_16 <- c()
    Preference_17 <- c()
    Preference_18 <- c()
    Preference_19 <- c()
    Preference_20 <- c()
    
    for(i in 1:input$total_voters){
      # RESET VARIABLES EACH LOOP
      distance_1 <- ""
      distance_2 <- ""
      distance_3 <- ""
      distance_4 <- ""
      distance_5 <- ""
      distance_6 <- ""
      distance_7 <- ""
      distance_8 <- ""
      distance_9 <- ""
      distance_10 <- ""
      distance_11 <- ""
      distance_12 <- ""
      distance_13 <- ""
      distance_14 <- ""
      distance_15 <- ""
      distance_16 <- ""
      distance_17 <- ""
      distance_18 <- ""
      distance_19 <- ""
      distance_20 <- ""
      
      # WRITE NEW USER-SPECIFIC VARIABLES (BASED ON # OF CANDIDATES)
      select_voter <- voter_data_with_distance[i,]
      distance_1 <- as.double(select_voter[3])
      if(input$candidate_count>=2){distance_2 <- as.double(select_voter[4])}
      if(input$candidate_count>=3){distance_3 <- as.double(select_voter[5])}
      if(input$candidate_count>=4){distance_4 <- as.double(select_voter[6])}
      if(input$candidate_count>=5){distance_5 <- as.double(select_voter[7])}
      if(input$candidate_count>=6){distance_6 <- as.double(select_voter[8])}
      if(input$candidate_count>=7){distance_7 <- as.double(select_voter[9])}
      if(input$candidate_count>=8){distance_8 <- as.double(select_voter[10])}
      if(input$candidate_count>=9){distance_9 <- as.double(select_voter[11])}
      if(input$candidate_count>=10){distance_10 <- as.double(select_voter[12])}
      if(input$candidate_count>=11){distance_11 <- as.double(select_voter[13])}
      if(input$candidate_count>=12){distance_12 <- as.double(select_voter[14])}
      if(input$candidate_count>=13){distance_13 <- as.double(select_voter[15])}
      if(input$candidate_count>=14){distance_14 <- as.double(select_voter[16])}
      if(input$candidate_count>=15){distance_15 <- as.double(select_voter[17])}
      if(input$candidate_count>=16){distance_16 <- as.double(select_voter[18])}
      if(input$candidate_count>=17){distance_17 <- as.double(select_voter[19])}
      if(input$candidate_count>=18){distance_18 <- as.double(select_voter[20])}
      if(input$candidate_count>=19){distance_19 <- as.double(select_voter[21])}
      if(input$candidate_count>=20){distance_20 <- as.double(select_voter[22])}
      # create arrow of candidate rankings (easier to have 20 columns, even if there are less than 20 candidates, can subset later if needed)
      voter_distance_array <- c(distance_1, distance_2, distance_3, distance_4, distance_5,
                                distance_6, distance_7, distance_8, distance_9, distance_10,
                                distance_11, distance_12, distance_13, distance_14, distance_15,
                                distance_16, distance_17, distance_18, distance_19, distance_20)
      voter_distance_ordered_table <- tibble(as.character(c(seq(1:20))),voter_distance_array) %>%
        arrange(as.numeric(voter_distance_array))
      voter_p1 <- as.character(voter_distance_ordered_table[1,1]) # rank 1
      voter_p2 <- as.character(voter_distance_ordered_table[2,1]) # rank 2
      voter_p3 <- as.character(voter_distance_ordered_table[3,1]) # rank 3
      voter_p4 <- as.character(voter_distance_ordered_table[4,1]) # rank 4
      voter_p5 <- as.character(voter_distance_ordered_table[5,1]) # rank 5
      voter_p6 <- as.character(voter_distance_ordered_table[6,1]) # rank 6
      voter_p7 <- as.character(voter_distance_ordered_table[7,1]) # rank 7
      voter_p8 <- as.character(voter_distance_ordered_table[8,1]) # rank 8
      voter_p9 <- as.character(voter_distance_ordered_table[9,1]) # rank 9
      voter_p10 <- as.character(voter_distance_ordered_table[10,1]) # rank 10
      voter_p11 <- as.character(voter_distance_ordered_table[11,1]) # rank 11
      voter_p12 <- as.character(voter_distance_ordered_table[12,1]) # rank 12
      voter_p13 <- as.character(voter_distance_ordered_table[13,1]) # rank 13
      voter_p14 <- as.character(voter_distance_ordered_table[14,1]) # rank 14
      voter_p15 <- as.character(voter_distance_ordered_table[15,1]) # rank 15
      voter_p16 <- as.character(voter_distance_ordered_table[16,1]) # rank 16
      voter_p17 <- as.character(voter_distance_ordered_table[17,1]) # rank 17
      voter_p18 <- as.character(voter_distance_ordered_table[18,1]) # rank 18
      voter_p19 <- as.character(voter_distance_ordered_table[19,1]) # rank 19
      voter_p20 <- as.character(voter_distance_ordered_table[20,1]) # rank 20
      
      Preference_1 <- append(Preference_1,
                             voter_p1,
                             after = length(Preference_1))
      Preference_2 <- append(Preference_2,
                             voter_p2,
                             after = length(Preference_2))
      Preference_3 <- append(Preference_3,
                             voter_p3,
                             after = length(Preference_3))
      Preference_4 <- append(Preference_4,
                             voter_p4,
                             after = length(Preference_4))
      Preference_5 <- append(Preference_5,
                             voter_p5,
                             after = length(Preference_5))
      Preference_6 <- append(Preference_6,
                             voter_p6,
                             after = length(Preference_6))
      Preference_7 <- append(Preference_7,
                             voter_p7,
                             after = length(Preference_7))
      Preference_8 <- append(Preference_8,
                             voter_p8,
                             after = length(Preference_8))
      Preference_9 <- append(Preference_9,
                             voter_p9,
                             after = length(Preference_9))
      Preference_10 <- append(Preference_10,
                              voter_p10,
                              after = length(Preference_10))
      Preference_11 <- append(Preference_11,
                              voter_p11,
                              after = length(Preference_11))
      Preference_12 <- append(Preference_12,
                              voter_p12,
                              after = length(Preference_12))
      Preference_13 <- append(Preference_13,
                              voter_p13,
                              after = length(Preference_13))
      Preference_14 <- append(Preference_14,
                              voter_p14,
                              after = length(Preference_14))
      Preference_15 <- append(Preference_15,
                              voter_p15,
                              after = length(Preference_15))
      Preference_16 <- append(Preference_16,
                              voter_p16,
                              after = length(Preference_16))
      Preference_17 <- append(Preference_17,
                              voter_p17,
                              after = length(Preference_17))
      Preference_18 <- append(Preference_18,
                              voter_p18,
                              after = length(Preference_18))
      Preference_19 <- append(Preference_19,
                              voter_p19,
                              after = length(Preference_19))
      Preference_20 <- append(Preference_20,
                              voter_p20,
                              after = length(Preference_20))
    }
    # output <- (voter_p2)
    output <- tibble(Preference_1, Preference_2, Preference_3, Preference_4, Preference_5,
                     Preference_6, Preference_7, Preference_8, Preference_9, Preference_10,
                     Preference_11, Preference_12, Preference_13, Preference_14, Preference_15,
                     Preference_16, Preference_17, Preference_18, Preference_19, Preference_20)
    return(output)
  }
  
  combined_data <- reactive({
    tibble(voterData(),as_tibble(candidate_distance()))
  })
  
  preference_ranker <- reactive({
    preference_ranker_function(combined_data())
  })
  
  rank_choice_data <-reactive({
    tibble(combined_data(), preference_ranker())
  })
  
  plurality_summary_table <- reactive({
    tibble(aggregate(rank_choice_data()$Preference_1,
                     by=list(rank_choice_data()$Preference_1),
                     length)) %>%  # View vote counts
      arrange(desc(x)) %>% 
      rename("Candidate" = "Group.1",
             "Votes" = "x")
  })
  
  # score candidates by distance
  scoring_function <- reactive({
    candidate_1_scoring <- ''
    candidate_2_scoring <- ''
    candidate_3_scoring <- ''
    candidate_4_scoring <- ''
    candidate_5_scoring <- ''
    candidate_6_scoring <- ''
    candidate_7_scoring <- ''
    candidate_8_scoring <- ''
    candidate_9_scoring <- ''
    candidate_10_scoring <- ''
    candidate_11_scoring <- ''
    candidate_12_scoring <- ''
    candidate_13_scoring <- ''
    candidate_14_scoring <- ''
    candidate_15_scoring <- ''
    candidate_16_scoring <- ''
    candidate_17_scoring <- ''
    candidate_18_scoring <- ''
    candidate_19_scoring <- ''
    candidate_20_scoring <- ''
    
    
    candidate_1_scoring <- mean(rank_choice_data()[,3,1],na.rm=T)
    if(input$candidate_count>=2){candidate_2_scoring <- mean(rank_choice_data()[,4,1],na.rm=T)}
    if(input$candidate_count>=3){candidate_3_scoring <- mean(rank_choice_data()[,5,1])}
    if(input$candidate_count>=4){candidate_4_scoring <- mean(rank_choice_data()[,6,1])}
    if(input$candidate_count>=5){candidate_5_scoring <- mean(rank_choice_data()[,7,1])}
    if(input$candidate_count>=6){candidate_6_scoring <- mean(rank_choice_data()[,8,1])}
    if(input$candidate_count>=7){candidate_7_scoring <- mean(rank_choice_data()[,9,1])}
    if(input$candidate_count>=8){candidate_8_scoring <- mean(rank_choice_data()[,10,1])}
    if(input$candidate_count>=9){candidate_9_scoring <- mean(rank_choice_data()[,11,1])}
    if(input$candidate_count>=10){candidate_10_scoring <- mean(rank_choice_data()[,12,1])}
    if(input$candidate_count>=11){candidate_11_scoring <- mean(rank_choice_data()[,13,1])}
    if(input$candidate_count>=12){candidate_12_scoring <- mean(rank_choice_data()[,14,1])}
    if(input$candidate_count>=13){candidate_13_scoring <- mean(rank_choice_data()[,15,1])}
    if(input$candidate_count>=14){candidate_14_scoring <- mean(rank_choice_data()[,16,1])}
    if(input$candidate_count>=15){candidate_15_scoring <- mean(rank_choice_data()[,17,1])}
    if(input$candidate_count>=16){candidate_16_scoring <- mean(rank_choice_data()[,18,1])}
    if(input$candidate_count>=17){candidate_17_scoring <- mean(rank_choice_data()[,19,1])}
    if(input$candidate_count>=18){candidate_18_scoring <- mean(rank_choice_data()[,20,1])}
    if(input$candidate_count>=19){candidate_19_scoring <- mean(rank_choice_data()[,21,1])}
    if(input$candidate_count>=20){candidate_20_scoring <- mean(rank_choice_data()[,22,1])}
    
    score_array<- c(candidate_1_scoring, candidate_2_scoring, candidate_3_scoring, candidate_4_scoring, candidate_5_scoring,
                    candidate_6_scoring, candidate_7_scoring, candidate_8_scoring, candidate_9_scoring, candidate_10_scoring,
                    candidate_11_scoring, candidate_12_scoring, candidate_13_scoring, candidate_14_scoring, candidate_15_scoring,
                    candidate_16_scoring, candidate_17_scoring, candidate_18_scoring, candidate_19_scoring, candidate_20_scoring)
    candidate_labels <- as.character(seq(1:20))
    score_table_ordered <- tibble(score_array, candidate_labels) %>% 
      arrange(as.double(score_array))
    head(score_table_ordered,input$candidate_count)
  })
  
  # approval voting decision making
  approval_number <- 50 # check if within 50 distance of 
  approval_check <- function(input, approval_number){
    if(input<=approval_number){
      return("approve")
    }
    else{
      "disapprove"
    }
  }
  approval_function <- function(voter_data_with_distance){
    candidate_1_approval <- c()
    candidate_2_approval <- c()
    candidate_3_approval <- c()
    candidate_4_approval <- c()
    candidate_5_approval <- c()
    candidate_6_approval <- c()
    candidate_7_approval <- c()
    candidate_8_approval <- c()
    candidate_9_approval <- c()
    candidate_10_approval <- c()
    candidate_11_approval <- c()
    candidate_12_approval <- c()
    candidate_13_approval <- c()
    candidate_14_approval <- c()
    candidate_15_approval <- c()
    candidate_16_approval <- c()
    candidate_17_approval <- c()
    candidate_18_approval <- c()
    candidate_19_approval <- c()
    candidate_20_approval <- c()
    
    # for(i in 1:2){
    for(i in 1:input$total_voters){
      # RESET VARIABLES EACH LOOP
      approval_1 <- c("")
      approval_2 <- c("")
      approval_3 <- c("")
      approval_4 <- c("")
      approval_5 <- c("")
      approval_6 <- c("")
      approval_7 <- c("")
      approval_8 <- c("")
      approval_9 <- c("")
      approval_10 <- c("")
      approval_11 <- c("")
      approval_12 <- c("")
      approval_13 <- c("")
      approval_14 <- c("")
      approval_15 <- c("")
      approval_16 <- c("")
      approval_17 <- c("")
      approval_18 <- c("")
      approval_19 <- c("")
      approval_20 <- c("")
      
      # WRITE NEW USER-SPECIFIC VARIABLES (BASED ON # OF CANDIDATES)
      select_voter <- voter_data_with_distance[i,]
      approval_1 <- approval_check(as.double(select_voter[3]),approval_number)
      if(input$candidate_count>=2){approval_2 <- approval_check(as.double(select_voter[4]),approval_number)}
      if(input$candidate_count>=3){approval_3 <- approval_check(as.double(select_voter[5]),approval_number)}
      if(input$candidate_count>=4){approval_4 <- approval_check(as.double(select_voter[6]),approval_number)}
      if(input$candidate_count>=5){approval_5 <- approval_check(as.double(select_voter[7]),approval_number)}
      if(input$candidate_count>=6){approval_6 <- approval_check(as.double(select_voter[8]),approval_number)}
      if(input$candidate_count>=7){approval_7 <- approval_check(as.double(select_voter[9]),approval_number)}
      if(input$candidate_count>=8){approval_8 <- approval_check(as.double(select_voter[10]),approval_number)}
      if(input$candidate_count>=9){approval_9 <- approval_check(as.double(select_voter[11]),approval_number)}
      if(input$candidate_count>=10){approval_10 <- approval_check(as.double(select_voter[12]),approval_number)}
      if(input$candidate_count>=11){approval_11 <- approval_check(as.double(select_voter[13]),approval_number)}
      if(input$candidate_count>=12){approval_12 <- approval_check(as.double(select_voter[14]),approval_number)}
      if(input$candidate_count>=13){approval_13 <- approval_check(as.double(select_voter[15]),approval_number)}
      if(input$candidate_count>=14){approval_14 <- approval_check(as.double(select_voter[16]),approval_number)}
      if(input$candidate_count>=15){approval_15 <- approval_check(as.double(select_voter[17]),approval_number)}
      if(input$candidate_count>=16){approval_16 <- approval_check(as.double(select_voter[18]),approval_number)}
      if(input$candidate_count>=17){approval_17 <- approval_check(as.double(select_voter[19]),approval_number)}
      if(input$candidate_count>=18){approval_18 <- approval_check(as.double(select_voter[20]),approval_number)}
      if(input$candidate_count>=19){approval_19 <- approval_check(as.double(select_voter[21]),approval_number)}
      if(input$candidate_count>=20){approval_20 <- approval_check(as.double(select_voter[22]),approval_number)}
      
      # voter_approval_array <- c(approval_1, approval_2, approval_3, approval_4, approval_5,
      #                           approval_6, approval_7, approval_8, approval_9, approval_10,
      #                           approval_11, approval_12, approval_13, approval_14, approval_15,
      #                           approval_16, approval_17, approval_18, approval_19, approval_20)
      # voter_approval_table <- tibble(as.character(c(seq(1:20))),voter_approval_array)
      # voter_approval_table
      
      candidate_1_approval <- append(candidate_1_approval,
                                     approval_1,
                                     after = length(candidate_1_approval))
      candidate_2_approval <- append(candidate_2_approval,
                                     approval_2,
                                     after = length(candidate_2_approval))
      candidate_3_approval <- append(candidate_3_approval,
                                     approval_3,
                                     after = length(candidate_3_approval))
      candidate_4_approval <- append(candidate_4_approval,
                                     approval_4,
                                     after = length(candidate_4_approval))
      candidate_5_approval <- append(candidate_5_approval,
                                     approval_5,
                                     after = length(candidate_5_approval))
      candidate_6_approval <- append(candidate_6_approval,
                                     approval_6,
                                     after = length(candidate_6_approval))
      candidate_7_approval <- append(candidate_7_approval,
                                     approval_7,
                                     after = length(candidate_7_approval))
      candidate_8_approval <- append(candidate_8_approval,
                                     approval_8,
                                     after = length(candidate_8_approval))
      candidate_9_approval <- append(candidate_9_approval,
                                     approval_9,
                                     after = length(candidate_9_approval))
      candidate_10_approval <- append(candidate_10_approval,
                                      approval_10,
                                      after = length(candidate_10_approval))
      candidate_11_approval <- append(candidate_11_approval,
                                      approval_11,
                                      after = length(candidate_11_approval))
      candidate_12_approval <- append(candidate_12_approval,
                                      approval_12,
                                      after = length(candidate_12_approval))
      candidate_13_approval <- append(candidate_13_approval,
                                      approval_13,
                                      after = length(candidate_13_approval))
      candidate_14_approval <- append(candidate_14_approval,
                                      approval_14,
                                      after = length(candidate_14_approval))
      candidate_15_approval <- append(candidate_15_approval,
                                      approval_15,
                                      after = length(candidate_15_approval))
      candidate_16_approval <- append(candidate_16_approval,
                                      approval_16,
                                      after = length(candidate_16_approval))
      candidate_17_approval <- append(candidate_17_approval,
                                      approval_17,
                                      after = length(candidate_17_approval))
      candidate_18_approval <- append(candidate_18_approval,
                                      approval_18,
                                      after = length(candidate_18_approval))
      candidate_19_approval <- append(candidate_19_approval,
                                      approval_19,
                                      after = length(candidate_19_approval))
      candidate_20_approval <- append(candidate_20_approval,
                                      approval_20,
                                      after = length(candidate_20_approval))
    }
    approval_output <- tibble(candidate_1_approval, candidate_2_approval, candidate_3_approval, candidate_4_approval, candidate_5_approval,
                              candidate_6_approval, candidate_7_approval, candidate_8_approval, candidate_9_approval, candidate_10_approval,
                              candidate_11_approval, candidate_12_approval, candidate_13_approval, candidate_14_approval, candidate_15_approval,
                              candidate_16_approval, candidate_17_approval, candidate_18_approval, candidate_19_approval, candidate_20_approval)
    
    # return(approval_output)
    return(approval_output[1:input$candidate_count])
  }
  candidate_approval_function <- reactive({
    approval_function(combined_data())
  })
  approval_summary_table <- reactive({
    tibble(as.character(seq(1:input$candidate_count)),
           c(
             candidate_approval_function() %>% filter(candidate_1_approval == 'approve') %>% nrow(),
             if(input$candidate_count>=2){
               candidate_approval_function() %>% filter(candidate_2_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=3){
               candidate_approval_function() %>% filter(candidate_3_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=4){
               candidate_approval_function() %>% filter(candidate_4_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=5){
               candidate_approval_function() %>% filter(candidate_5_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=6){
               candidate_approval_function() %>% filter(candidate_6_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=7){
               candidate_approval_function() %>% filter(candidate_7_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=8){
               candidate_approval_function() %>% filter(candidate_8_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=9){
               candidate_approval_function() %>% filter(candidate_9_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=10){
               candidate_approval_function() %>% filter(candidate_10_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=11){
               candidate_approval_function() %>% filter(candidate_11_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=12){
               candidate_approval_function() %>% filter(candidate_12_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=13){
               candidate_approval_function() %>% filter(candidate_13_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=14){
               candidate_approval_function() %>% filter(candidate_14_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=15){
               candidate_approval_function() %>% filter(candidate_15_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=16){
               candidate_approval_function() %>% filter(candidate_16_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=17){
               candidate_approval_function() %>% filter(candidate_17_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=18){
               candidate_approval_function() %>% filter(candidate_18_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=19){
               candidate_approval_function() %>% filter(candidate_19_approval == 'approve') %>% nrow()},
             if(input$candidate_count>=20){
               candidate_approval_function() %>% filter(candidate_20_approval == 'approve') %>% nrow()}
           )
    ) %>% 
      rename('Candidate' = 'as.character(seq(1:input$candidate_count))',
             'Approved'='c(...)')
  })
  
  
  # rank choice voting process another round 
  rank_choice_run_off_function <- function(voter_rankings, majority_check){
    selected_rankings <- aggregate(rc_preference_table[,rank_choice_results_passes,1], 
                                   by=list(rc_preference_table[,rank_choice_results_passes,1]),
                                   length) 
    selected_data <- ifelse(rc_preference_table[,(rank_choice_results_passes-1),1]==as.character(previous_loser),
                            rc_preference_table[,(rank_choice_results_passes),1],
                            rc_preference_table[,(rank_choice_results_passes-1),1])
    selected_results <- aggregate(selected_data, by=list(selected_data), length) %>%
      rename("Candidate" = "Group.1",
             "Votes" = "x")
    selected_results <- selected_results %>% 
      arrange(desc(as.numeric(Votes)))
    return(selected_results)
  }
  
  # return ranked-choice data
  ranked_choice_data_processing <- reactive({
    rc_1 <- rank_choice_data()$Preference_1
    rc_2 <- rank_choice_data()$Preference_2
    rc_3 <- rank_choice_data()$Preference_3
    rc_4 <- rank_choice_data()$Preference_4
    rc_5 <- rank_choice_data()$Preference_5
    rc_6 <- rank_choice_data()$Preference_6
    rc_7 <- rank_choice_data()$Preference_7
    rc_8 <- rank_choice_data()$Preference_8
    rc_9 <- rank_choice_data()$Preference_9
    rc_10 <- rank_choice_data()$Preference_10
    rc_11 <- rank_choice_data()$Preference_11
    rc_12 <- rank_choice_data()$Preference_12
    rc_13 <- rank_choice_data()$Preference_13
    rc_14 <- rank_choice_data()$Preference_14
    rc_15 <- rank_choice_data()$Preference_15
    rc_16 <- rank_choice_data()$Preference_16
    rc_17 <- rank_choice_data()$Preference_17
    rc_18 <- rank_choice_data()$Preference_18
    rc_19 <- rank_choice_data()$Preference_19
    rc_20 <- rank_choice_data()$Preference_20
    
    rc_preference_table <- tibble(rc_1, rc_2, rc_3, rc_4, rc_5,
                                  rc_6, rc_7, rc_8, rc_9, rc_10,
                                  rc_11, rc_12, rc_13, rc_14, rc_15,
                                  rc_16, rc_17, rc_18, rc_19, rc_20)
    
    rank_choice_results_passes <- 1 # start at 1
    majority_check <- ceiling(input$total_voters/2) # round up to nearest integer
    
    first_round_data <- aggregate(rc_preference_table[,rank_choice_results_passes,1], 
                                  by=list(rc_preference_table[,rank_choice_results_passes,1]),
                                  length) %>%
      rename("Candidate" = "Group.1",
             "Votes" = "x") 
    first_round_data <- first_round_data %>% 
      arrange(desc(as.numeric(Votes)))
    first_round_leader <- first_round_data[1,2]
    first_round_loser <- tail(first_round_data,1)[1]
    
    
    
    if(!first_round_data[1,2]>=majority_check){
      rank_choice_results_passes <- rank_choice_results_passes+1
      previous_loser <- first_round_loser
    }
    
    # first_round
    # majority_check
    # first_round[1,2]>=majority_check
    
    # if(rank_choice_results_passes==2){} # run function inside of this if/statement (repeat 19x times)
    
    
    
    
    
    # if(first_round_leader>=majority_check){
    #   second_round_data <- ifelse(rank_choice_data()$Preference_1==as.character(first_round_loser), 
    #                               rank_choice_data()$Preference_2, 
    #                               rank_choice_data()$Preference_1)
    #   second_round_results <- aggregate(second_round_data, by=list(second_round_data), length) %>% 
    #     rename("Candidate" = "Group.1",
    #            "Votes" = "x")
    #   second_round_results <- second_round_results %>% 
    #     arrange(desc(as.numeric(Votes)))
    #   second_round_leader <- second_round_results[1,2]
    #   second_round_loser <- tail(second_round_results,1)[1]
    # }
    
    
    
  })
  
  #-----------------------------------------------------------------------------------------
  # Create plots
  pt1_plurality <- reactive({
    if (!input$voting_system=='plurality') return(NULL)
    ggplot() +
      # Add voter data points
      geom_point(data=rank_choice_data(), 
                 aes(x=Voter_Dim_1(), y=Voter_Dim_2(), 
                     size=1.1, alpha=.95, color=factor(Preference_1 ))) +  
      # Add candidate data points
      geom_point(data=candidateData(),
                 aes(x=Candidate_Dim_1(), y=Candidate_Dim_2(),
                     size=1.3, alpha=.9, color=Candidate_Names())) +
      # Add candidate labels
      annotate("text", x=Candidate_Dim_1(), y=Candidate_Dim_2()+1, label=as.character(Candidate_Names()),
               fontface=2, hjust=.5, color='#000000') +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # axis scaling
      scale_x_continuous(limits=c(-100,100)) +
      scale_y_continuous(limits=c(-100,101)) +
      # label axes
      labs(x="Economic Scale", y="Social Scale", title='Voter & Candidates Political Leanings')
  })
  
  pt1_ranked_choice <- reactive({
    if (!input$voting_system=='ranked_choice') return(NULL)
    ggplot() +
      # Add voter data points
      geom_point(data=rank_choice_data(), 
                 aes(x=Voter_Dim_1(), y=Voter_Dim_2(), 
                     size=1.1, alpha=.95, color=factor(Preference_1 ))) +  
      # Add candidate data points
      geom_point(data=candidateData(),
                 aes(x=Candidate_Dim_1(), y=Candidate_Dim_2(),
                     size=1.3, alpha=.9, color=Candidate_Names())) +
      # Add candidate labels
      annotate("text", x=Candidate_Dim_1(), y=Candidate_Dim_2()+1, label=as.character(Candidate_Names()),
               fontface=2, hjust=.5, color='#000000') +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # axis scaling
      scale_x_continuous(limits=c(-100,100)) +
      scale_y_continuous(limits=c(-100,101)) +
      # label axes
      labs(x="Economic Scale", y="Social Scale", title='Voter & Candidates Political Leanings')
  })
  
  pt1_score <- reactive({
    if (!input$voting_system=='score') return(NULL)
    ggplot() +
      # Add voter data points
      geom_point(data=rank_choice_data(), 
                 aes(x=Voter_Dim_1(), y=Voter_Dim_2(), 
                     size=1.1, alpha=.95, color=factor(Preference_1))) +  
      # Add candidate data points
      geom_point(data=candidateData(),
                 aes(x=Candidate_Dim_1(), y=Candidate_Dim_2(),
                     size=1.3, alpha=.9, color=Candidate_Names())) +
      # Add candidate labels
      annotate("text", x=Candidate_Dim_1(), y=Candidate_Dim_2()+1, label=as.character(Candidate_Names()),
               fontface=2, hjust=.5, color='#000000') +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # axis scaling
      scale_x_continuous(limits=c(-100,100)) +
      scale_y_continuous(limits=c(-100,101)) +
      # label axes
      labs(x="Economic Scale", y="Social Scale", title='Voter & Candidates Political Leanings')
  })
  
  # functions for drawing circles
  draw_circles <- reactive({
    data.frame(
      x0 = rep(1, input$candidate_count),
      y0 = rep(1, each = input$candidate_count),
      r = seq(0.8, 1, length.out = 1)
    )
  })
  draw_circles_function <- function(x, y, color, fill){
    list(geom_circle(data=draw_circles(), aes(
      x0 = unlist(as.double(x)),
      y0 = unlist(as.double(y)),
      color= unlist(color),
      r = unlist(approval_number),
      fill = unlist(factor(fill)))))
  }
  pt1_approval <- reactive({
    if (!input$voting_system=='approval') return(NULL)
    ggplot() +
      # add circles
      geom_circle(data=draw_circles(), aes(
        x0 = unlist(candidateData()[,1,1]),
        y0 = unlist(candidateData()[,2,1]),
        color= unlist(candidateData()[,3,1]),
        r = rep(approval_number,input$candidate_count), # size=50 for circles
        fill = unlist(candidateData()[,3,1]),
        alpha=rep(.6,input$candidate_count))) +
      # Add voter data points
      geom_point(data=rank_choice_data(), 
                 aes(x=Voter_Dim_1(), y=Voter_Dim_2(), 
                     size=1.1, alpha=.95, fill='black')) +  
      # Add candidate data points
      geom_point(data=candidateData(),
                 aes(x=Candidate_Dim_1(), y=Candidate_Dim_2(),
                     size=1.3, alpha=.9, color=Candidate_Names())) +
      # Add candidate labels
      annotate("text", x=Candidate_Dim_1(), y=Candidate_Dim_2()+1, label=as.character(Candidate_Names()),
               fontface=2, hjust=.5, color='#000000') +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # axis scaling
      scale_x_continuous(limits=c(-151,151)) +
      scale_y_continuous(limits=c(-151,151)) +
      # label axes
      labs(x="Economic Scale", y="Social Scale", title='Voter & Candidates Political Leanings') +
      coord_cartesian(ylim = c(-100, 100), xlim = c(-100,100)) # this line 'zooms' in plot view so that entire circles can be drawn, and latter cropped
  })
  
  pt2_plurality <- reactive({
    if (!input$voting_system=='plurality') return(NULL)
    ggplot()+
      geom_bar(data=plurality_summary_table(), stat='identity', aes(x=as.integer(as.character(Candidate)), y=Votes, fill=factor(Candidate))) +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # label axes
      labs(title='Plurality Results', x='Candidates') +
      # scale y-axis based on vote total
      scale_y_continuous(limits=c(0,input$total_voters)) +
      #scale x-axis based on # of candidates
      scale_x_continuous(labels=Candidate_Names(), 
                         breaks=as.integer(as.character(Candidate_Names()))) +
      # add dotted line at 50% mark
      geom_hline(yintercept = round(input$total_voters)/2, 0, linetype='dashed')
  })
  
  pt2_ranked_choice <- reactive({
    if (!input$voting_system=='ranked_choice') return(NULL)
    ggplot()+
      geom_bar(data=plurality_summary_table(), stat='identity', aes(x=as.integer(as.character(Candidate)), y=Votes, fill=factor(Candidate))) +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # label axes
      labs(title='Ranked-Choice Results', x='Candidates') +
      # scale y-axis based on vote total
      scale_y_continuous(limits=c(0,input$total_voters)) +
      #scale x-axis based on # of candidates
      scale_x_continuous(labels=Candidate_Names(), 
                         breaks=as.integer(as.character(Candidate_Names()))) +
      # add dotted line at 50% mark
      geom_hline(yintercept = round(input$total_voters)/2, 0, linetype='dashed')
  })
  
  # note: similarity score is max distance score ("282.8427") - average(distance) [this is so most similar scores (low #'s) are highest bars] 
  pt2_score <- reactive({
    if (!input$voting_system=='score') return(NULL)
    ggplot()+
      geom_bar(data=scoring_function(), stat='identity', aes(x=as.integer(as.character(candidate_labels)), y=282.8427-as.double(score_array), fill=factor(candidate_labels))) +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # label axes
      labs(title='Score Results', x='Candidates', y='Similarity Score') +
      # scale y-axis based on vote total
      scale_y_continuous(limits=c(0,300)) +
      scale_x_continuous(labels=Candidate_Names(), 
                         breaks=as.integer(as.character(Candidate_Names())))
    
  })
  
  pt2_approval <- reactive({
    if (!input$voting_system=='approval') return(NULL)
    ggplot()+
      geom_bar(data=approval_summary_table(), stat='identity', aes(x=as.integer(as.character(Candidate)), y=as.double(Approved), fill=factor(Candidate))) +
      # theme changes
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # label axes
      labs(title='Approval Results', x='Candidates', y='Approval Count') +
      # scale y-axis based on vote total
      scale_y_continuous(limits=c(0,input$total_voters)) +
      scale_x_continuous(labels=Candidate_Names(),
                         breaks=as.integer(as.character(Candidate_Names())))
    
  })
  
  # render plots
  output$plotgraph = renderPlot({
    ptlist <- list(pt1_plurality(), pt2_plurality(),
                   pt1_ranked_choice(), pt2_ranked_choice(),
                   pt1_score(), pt2_score(),
                   pt1_approval(), pt2_approval())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,
                 ncol=2)
  })
  
  
  observeEvent(rank_choice_data(),
               # print(rank_choice_data()))
               print(combined_data()))
  # observeEvent(rank_choice_data(),
  #              # print(rank_choice_data()[3:(2+input$candidate_count)]))
  #              print(ranked_choice_data_processing()))
  
  # print(candidateData()[,1,1]))
  # print(approval_summary_table()))
  #              # print(mean(rank_choice_data()[,3,1])))
  
})
#-----------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
