conditionalPanel(condition = "input.mainPanel == 'information' && input.info_panels == 'admin_panel'",
                 textPassword(inputId='admin_code', 'Enter admin. code', value=''),
                 myHiddenBoolean(inputId='admin_show', label='Show admin panel', value=FALSE)
)