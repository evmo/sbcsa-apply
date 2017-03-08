# {{input$team_name}}
## {{ ifelse(input$start=="not listed here", input$custom_route, paste(input$start, "-", input$finish)) }}
## {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%B %d, %Y")}}

---

### Team Leader

Name | Email | Phone
---- | ----- | -----
{{input$s_name}} | {{input$s_email}} | {{input$s_phone}}

{{input$s_mailing}}

### Team Members

Name | Email
---- | -----
{{input$member_name1}} | {{input$member_email1}}
{{input$member_name2}} | {{input$member_email2}}
{{ if (input$team_size >= 3) paste(input$member_name3, "|", input$member_email3) }}
{{ if (input$team_size >= 4) paste(input$member_name4, "|", input$member_email4) }}
{{ if (input$team_size >= 5) paste(input$member_name5, "|", input$member_email5) }}
{{ if (input$team_size >= 6) paste(input$member_name6, "|", input$member_email6) }}

{{ if (input$alt_size > 0) h4("Alternates") }}

Name | Email
---- | -----
{{ if (input$alt_size >= 1) paste(input$alt_name1, "|", input$alt_email1) }}
{{ if (input$alt_size == 2) paste(input$alt_name2, "|", input$alt_email2) }}

**Leg Duration**: {{ input$leg_duration }} minutes.

### Escort Boat

- **Boat:** {{ ifelse(input$boat_known=="BOAT NOT LISTED", input$boat_other, input$boat_known) }}
- **Depart harbor:** {{format(as.Date(input$harbor_date, origin="1970-01-01"), "%a %b %d %Y")}}, {{paste0(input$harbor_time, ":00")}}
- **Swimmer in water:** {{format(input$splash_date, "%a %b %d %Y")}}, {{paste0(input$splash_time, ":00")}}

### Support Team

#### Crew Chief

Name | Email | Phone
---- | ----- | -----
{{input$cc_name}} | {{input$cc_email}} | {{input$cc_phone}}

#### Additional Crew

Name | Role | Contact
---- | ---- | -------
{{ if (rv$crew_count >= 1) input$crew_name1 }} | {{ if (rv$crew_count >= 1) input$crew_role1 }} | {{ if (rv$crew_count >= 1) input$crew_contact1 }}
{{ if (rv$crew_count >= 2) input$crew_name2 }} | {{ if (rv$crew_count >= 2) input$crew_role2 }} | {{ if (rv$crew_count >= 2) input$crew_contact2 }}
{{ if (rv$crew_count >= 3) input$crew_name3 }} | {{ if (rv$crew_count >= 3) input$crew_role3 }} | {{ if (rv$crew_count >= 3) input$crew_contact3 }}
{{ if (rv$crew_count >= 4) input$crew_name4 }} | {{ if (rv$crew_count >= 4) input$crew_role4 }} | {{ if (rv$crew_count >= 4) input$crew_contact4 }}
{{ if (rv$crew_count >= 5) input$crew_name5 }} | {{ if (rv$crew_count >= 5) input$crew_role5 }} | {{ if (rv$crew_count >= 5) input$crew_contact5 }}
