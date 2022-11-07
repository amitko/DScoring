library("DScoring")

# Load base test deltas
bt_deltas <- read.csv('base_test_deltas.csv',header = FALSE)

# Load new test deltas
nt_deltas <- read.csv('new_test_deltas.csv',header = FALSE)

# Load common items
common <- read.csv('common.csv',header = FALSE)

# Calculating rescaling constants
Const <- DS.equatingConstants(as.matrix(bt_deltas), as.matrix(nt_deltas), as.matrix(common))
Const$A
Const$B

# Rescaling deltas
RescaledDeltas <- DS.equatingRescale(nt_deltas,Const)
RescaledDeltas
