

bind(player_chat, null, null, @event) {
    @msg = @event['message']
    @idemp = time()
    export('mccommand.text', @msg)
    export('mccommand.idemp', @idemp)
}


bind(player_interact, null, null, @event) {
    if(@event['action'] == 'left_click_air') {
        @possibleLocationLookedAt = pcursor('nasuno') # Change me
        @observedLocation = concat(@possibleLocationLookedAt[0],',',@possibleLocationLookedAt[1],',',@possibleLocationLookedAt[2])
        @idemp = time()
        export('mouse.left.click', @observedLocation)
        export('mouse.left.idemp', @idemp)
    }
}

bind(player_interact, null, null, @event) {
    if(@event['action'] == 'right_click_air') {
        @possibleLocationLookedAt = pcursor('nasuno') # Change me
        @observedLocation = concat(@possibleLocationLookedAt[0],',',@possibleLocationLookedAt[1],',',@possibleLocationLookedAt[2])
        @idemp = time()
        export('mouse.right.click', @observedLocation)
        export('mouse.right.idemp', @idemp)
    }
}


/*
bind(block_place, null, null, @event,@hand,
	@coords = @event['location']
	msg('X= ', @coords[0])
	msg('Y= ', @coords[1])
	msg('Z= ', @coords[2])
	msg('world= ', @coords[3])
)
*/

