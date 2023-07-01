@spice
type language = | @spice.as("ReScript") ReScript

@spice
type profile = {languages?: array<language>}

@spice
type user = {profile: profile}
