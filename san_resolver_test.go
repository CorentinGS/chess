package chess

import "testing"

func TestResolveSANMoveDirectPawnPush(t *testing.T) {
	pos := StartingPosition()
	for _, dest := range []Square{E3, E4} {
		move, ok := resolveSANMoveDirect(pos, sanMoveData{piece: Pawn, dest: dest})
		if !ok || move.From() != E2 || move.To() != dest {
			t.Fatalf("resolve pawn push to %s = %v, %t; want e2 to %s", dest, move, ok, dest)
		}
	}
}
