package opening

import _ "embed"

//go:embed eco_lichess.tsv
var ecoData []byte

// TODO: change this so that it is not a global variable
