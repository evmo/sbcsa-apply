# {{input$s_name}}
## {{ ifelse(input$route=="Anacapa to mainland", "Anacapa to mainland", ifelse(input$alt_start=="not listed here", input$custom_route, paste(input$alt_start, "to", input$alt_finish))) }}
## {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%B %d, %Y")}}

---

Gender | D.O.B | Age on Swim Date | Citizen
------ | ----- | ---------------- | -------
{{
    gender <- input$s_gender
    age <- rv$swim_age
    citizen <- ifelse(input$other_citizen, 
                      input$s_citizenship, 
                      input$s_country)
    paste(gender, input$s_dob, age, citizen, sep = " | ")
}}

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

### Escort Boat

- **Boat:** {{input$boat}}
- **Depart harbor:** {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%a %b %d %Y")}}, {{paste0(input$harbor_time, ":00")}}
- **Swimmer in water:** {{format(input$splash_date, "%a %b %d %Y")}}, {{paste0(input$splash_time, ":00")}}

### Support Team

**Crew chief:** {{input$cc_name}} - {{input$cc_email}} - {{input$cc_phone}}

Name | Role | Contact
---- | ---- | -------
{{ if (rv$crew_count >= 1) input$crew_name1 }} | {{ if (rv$crew_count >= 1) input$crew_role1 }} | {{ if (rv$crew_count >= 1) input$crew_contact1 }}
{{ if (rv$crew_count >= 2) input$crew_name2 }} | {{ if (rv$crew_count >= 2) input$crew_role2 }} | {{ if (rv$crew_count >= 2) input$crew_contact2 }}
{{ if (rv$crew_count >= 3) input$crew_name3 }} | {{ if (rv$crew_count >= 3) input$crew_role3 }} | {{ if (rv$crew_count >= 3) input$crew_contact3 }}
{{ if (rv$crew_count >= 4) input$crew_name4 }} | {{ if (rv$crew_count >= 4) input$crew_role4 }} | {{ if (rv$crew_count >= 4) input$crew_contact4 }}
{{ if (rv$crew_count >= 5) input$crew_name5 }} | {{ if (rv$crew_count >= 5) input$crew_role5 }} | {{ if (rv$crew_count >= 5) input$crew_contact5 }}

### Swim Experience

Year | Swim | Distance | Duration | Temp
---- | ---- | -------- | -------- | ----
{{ if (rv$swim_count >= 1) input$swim_year1 }} | {{ if (rv$swim_count >= 1) input$swim_name1 }} | {{ if (rv$swim_count >= 1) input$swim_dist1 }} {{ if (rv$swim_count >= 1) input$swim_units1 }} | {{ if (rv$swim_count >= 1) paste0(input$swim_hr1, "hr") }} {{ if (rv$swim_count >= 1) paste0(input$swim_min1, "min") }}  | {{ if (rv$swim_count >= 1) input$swim_temp1 }}
{{ if (rv$swim_count >= 2) input$swim_year2 }} | {{ if (rv$swim_count >= 2) input$swim_name2 }} | {{ if (rv$swim_count >= 2) input$swim_dist2 }} {{ if (rv$swim_count >= 1) input$swim_units2 }} | {{ if (rv$swim_count >= 2) paste0(input$swim_hr2, "hr") }} {{ if (rv$swim_count >= 2) paste0(input$swim_min2, "min") }}  | {{ if (rv$swim_count >= 2) input$swim_temp2 }}
{{ if (rv$swim_count >= 3) input$swim_year3 }} | {{ if (rv$swim_count >= 3) input$swim_name3 }} | {{ if (rv$swim_count >= 3) input$swim_dist3 }} {{ if (rv$swim_count >= 1) input$swim_units3 }} | {{ if (rv$swim_count >= 3) paste0(input$swim_hr3, "hr") }} {{ if (rv$swim_count >= 3) paste0(input$swim_min3, "min") }}  | {{ if (rv$swim_count >= 3) input$swim_temp3 }}
{{ if (rv$swim_count >= 4) input$swim_year4 }} | {{ if (rv$swim_count >= 4) input$swim_name4 }} | {{ if (rv$swim_count >= 4) input$swim_dist4 }} {{ if (rv$swim_count >= 1) input$swim_units4 }} | {{ if (rv$swim_count >= 4) paste0(input$swim_hr4, "hr") }} {{ if (rv$swim_count >= 4) paste0(input$swim_min4, "min") }}  | {{ if (rv$swim_count >= 4) input$swim_temp4 }}
{{ if (rv$swim_count >= 5) input$swim_year5 }} | {{ if (rv$swim_count >= 5) input$swim_name5 }} | {{ if (rv$swim_count >= 5) input$swim_dist5 }} {{ if (rv$swim_count >= 1) input$swim_units5 }} | {{ if (rv$swim_count >= 5) paste0(input$swim_hr5, "hr") }} {{ if (rv$swim_count >= 5) paste0(input$swim_min5, "min") }}  | {{ if (rv$swim_count >= 5) input$swim_temp5 }}

#### More Details

{{input$background_details}}

### Feeding Plan

{{input$feeding_plan}}

#### Experience with feeding plan

{{input$feed_experience}}

### Other Swimmer Details

- **Stroke rate:** {{input$stroke_rate}}
- **Breathing pattern:** {{input$breathing}}
- **Hypothermia experience:** {{input$hypothermia}}
- **Night swimming experience:** {{input$night_swimming}}
- **Permission to Publicize:** {{input$publicize}}
- **Method of Payment:** {{input$payment_choice}}

### Medical

To the best of my knowledge, I am in excellent general health and have not omitted any information which might be relevant to my ability to swim the Santa Barbara Channel. **Yes**

I have been examined by a medical doctor within the past 12 months, and have been specifically cleared to undertake this event. **Yes**

Date of Medical Exam **{{input$med_date}}**

### Signature

{{input$waiver_sig}}