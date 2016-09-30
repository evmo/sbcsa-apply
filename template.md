# {{input$s_name}}
## {{ifelse(input$route=="Anacapa to mainland","Anacapa to mainland",input$route_other)}}
## {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%B %d, %Y")}}

---

Gender | D.O.B | Age on Swim Date | Citizen
------ | ----- | ---------------- | -------
{{input$s_gender}} | {{format(as.Date(input$s_dob, origin="1970-01-01"), "%Y-%b-%d")}} | {{input$s_name}} | {{input$s_citizenship}}

### Contact Info

Email | Phone | Alt. Phone | Address |
----- | ----- | ---------- | ------- |
{{input$s_email}} | {{input$s_phone}} | {{input$s_phone_alt}} | {{input$s_mailing}}

### Emergency Contact

Name | Email | Phone | Alt. Phone |
---- | ----- | ----- | ---------- |
{{input$ec_name}} | {{input$ec_email}} | {{input$ec_phone}} | {{input$ec_phone_alt}}

### Escort Boat

- **Boat:** {{input$escort_boat}}
- **Depart harbor:** {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%a %b %d %Y")}}, {{paste0(input$harbor_time, ":00")}}
- **Swimmer in water:** {{format(as.Date(input$splash_date, origin="1970-01-01"), "%a %b %d %Y")}}, {{paste0(input$splash_time, ":00")}}

### Support Team

**Crew chief:** {{input$cc_name}} - {{input$cc_email}} - {{input$cc_phone}}

Name | Role | Contact
---- | ---- | -------
{{input$crew_name1}} | {{input$crew_role1}} | {{input$crew_contact1}}
{{input$crew_name2}} | {{input$crew_role2}} | {{input$crew_contact2}}
{{input$crew_name3}} | {{input$crew_role3}} | {{input$crew_contact3}}

### Swim History

Year | Swim | Distance | Duration | Temp
---- | ---- | -------- | -------- | ----
{{input$swim_year1}} | {{input$swim_name1}} | {{input$swim_dist1}} | {{input$swim_dur1}} | {{input$swim_temp1}}
{{input$swim_year2}} | {{input$swim_name2}} | {{input$swim_dist2}} | {{input$swim_dur2}} | {{input$swim_temp2}}
{{input$swim_year3}} | {{input$swim_name3}} | {{input$swim_dist3}} | {{input$swim_dur3}} | {{input$swim_temp3}}
{{input$swim_year4}} | {{input$swim_name4}} | {{input$swim_dist4}} | {{input$swim_dur4}} | {{input$swim_temp4}}
{{input$swim_year5}} | {{input$swim_name5}} | {{input$swim_dist5}} | {{input$swim_dur5}} | {{input$swim_temp5}}

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

\-  |  \-
-- | --
To the best of my knowledge, I am in excellent general health and have not omitted any information which might be relevant to my ability to swim the Santa Barbara Channel. | Yes
I have been examined by a medical doctor within the past 12 months, and have been specifically cleared to undertake this event. | Yes
Date of Medical Exam | {{input$med_date}}

**Electronic signature on file.**