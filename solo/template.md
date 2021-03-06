# {{input$s_name}}
## {{ ifelse(input$start=="not listed here", input$custom_route, paste(input$start, "-", input$finish)) }}
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

Email | Phone | Address
----- | ----- | -------
{{input$s_email}} | {{input$s_phone}} | {{input$s_mailing}}
{{input$p_email}} | {{input$p_phone}} | {{input$p_mailing}}

### Emergency Contact

Name | Relationship | Email | Phone | 
---- | ----- | ----- | ---------- |
{{input$ec_name}} | {{input$ec_rel}} | {{input$ec_email}} | {{input$ec_phone}}

### Escort Boat

- **Boat:** {{ ifelse(input$boat_known=="BOAT NOT LISTED", input$boat_other, input$boat_known) }}
- **Depart harbor:** {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%a %b %d %Y")}}, {{input$harbor_time}}
- **Swimmer in water:** {{format(input$splash_date, "%a %b %d %Y")}}, {{input$splash_time}}

### Support Team

#### Crew Chief

Name | Email | Phone
---- | ----- | -----
{{input$cc_name}} | {{input$cc_email}} | {{input$cc_phone}}

Name | Role | Contact
---- | ---- | -------
{{ if (rv$crew_count >= 1) input$crew_name1 }} | {{ if (rv$crew_count >= 1) input$crew_role1 }} | {{ if (rv$crew_count >= 1) input$crew_contact1 }}
{{ if (rv$crew_count >= 2) input$crew_name2 }} | {{ if (rv$crew_count >= 2) input$crew_role2 }} | {{ if (rv$crew_count >= 2) input$crew_contact2 }}
{{ if (rv$crew_count >= 3) input$crew_name3 }} | {{ if (rv$crew_count >= 3) input$crew_role3 }} | {{ if (rv$crew_count >= 3) input$crew_contact3 }}
{{ if (rv$crew_count >= 4) input$crew_name4 }} | {{ if (rv$crew_count >= 4) input$crew_role4 }} | {{ if (rv$crew_count >= 4) input$crew_contact4 }}
{{ if (rv$crew_count >= 5) input$crew_name5 }} | {{ if (rv$crew_count >= 5) input$crew_role5 }} | {{ if (rv$crew_count >= 5) input$crew_contact5 }}

### Swim Experience

{{input$background_details}}

### Feeding Plan

{{input$feed_plan}}

#### Experience with feeding plan

{{input$feed_experience}}

### Other Swimmer Details

- **Stroke rate:** {{input$stroke_rate}}
- **Hypothermia experience:** {{input$hypothermia}}
- **Night swimming experience:** {{input$night_swimming}}
- **Permission to Publicize:** {{input$publicize}}
- **Current lifetime member:** {{input$current_lifetime}}
- **Purchase new lifetime?** {{input$lifetime_purchase}}
- **Method of Payment:** {{input$payment_choice}}

### Medical

To the best of my knowledge, I am in excellent general health and have not omitted any information which might be relevant to my ability to swim the Santa Barbara Channel. **Yes**

I have been examined by a medical doctor within the past 12 months, and have been specifically cleared to undertake this event. **Yes**

In the interest of the health and safety of participants in SBCSA-sanctioned swims in 2021, everyone aboard the escort boat - swimmer, crew, pilot, and observer - will be required to provide proof of COVID-19 vaccination **I understand and will comply with this requirement**

Date of Medical Exam **{{input$med_date}}**

### Signature

{{input$waiver_sig}}
