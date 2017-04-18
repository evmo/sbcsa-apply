## {{input$s_name}} - {{input$team_name}}

Gender | D.O.B | Age on Swim Date 
------ | ----- | ---------------- 
{{
    gender <- input$s_gender
    age <- rv$swim_age
    citizen <- ifelse(input$other_citizen, 
                      input$s_citizenship, 
                      input$s_country)
    paste(gender, input$s_dob, age, sep = " | ")
}}

- **Current lifetime member:** {{input$current_lifetime}}
- **Purchase new lifetime?** {{input$new_lifetime}}

### Contact Info

Email | Phone
----- | -----
{{input$s_email}} | {{input$s_phone}}
{{input$p_email}} | {{input$p_phone}}

{{input$s_mailing}}
{{input$p_mailing}}

### Emergency Contact

Name | Relationship | Email | Phone | 
---- | ----- | ----- | ---------- |
{{input$ec_name}} | {{input$ec_rel}} | {{input$ec_email}} | {{input$ec_phone}}

### Swim Experience

{{input$experience}}

### Medical

To the best of my knowledge, I am in excellent general health and have not omitted any information which might be relevant to my ability to swim the Santa Barbara Channel. **Yes**

I have been examined by a medical doctor within the past 12 months, and have been specifically cleared to undertake this event. **Yes**

Date of Medical Exam **{{input$med_date}}**

### Signature

{{input$waiver_sig}}
