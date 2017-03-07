fill_sample <- function(session) {
  updateTextInput(session, 's_name', value = 'Matthew Webb')
  updateTextInput(session, 's_email', value = 'mwebb@channelswimmingassocation.com')
  updateSelectInput(session, 's_gender', selected = 'male')
  updateTextInput(session, 's_phone', value = '505-867-5309')
  updateTextAreaInput(session, 's_mailing', value = '121 Main St, Dover, England')
  updateTextInput(session, 'team_name', value = 'Chunky Dunkers')
  updateSelectInput(session, 'team_size', selected = 6)
  updateSelectInput(session, 'boat_known', selected = 'Tuna Thumper')
  updateTextInput(session, 'cc_name', value = "Tabloid Terry")
  updateTextInput(session, 'cc_email', value = "tterry@hotmail.com")
}