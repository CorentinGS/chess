package chess

import "testing"

func TestResolveSANMoveDirectPawnPush(t *testing.T) {
	pos := StartingPosition()
	for _, dest := range []Square{E3, E4} {
		move, ok := resolveSANMoveDirect(pos, sanMoveData{piece: Pawn, dest: dest})
		if !ok || move.S1() != E2 || move.S2() != dest {
			t.Fatalf("resolve pawn push to %s = %v, %t; want e2 to %s", dest, move, ok, dest)
		}
	}
}
