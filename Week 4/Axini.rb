# Set the default timeout for responses (in seconds)
timeout 0.5

# Define an external channel
external 'door'

# Define a process describing the behavior of the coffee machine.
process('main') {

  # Define stimuli (input) and responses (output) the process can perform on
  # this channel.
  channel('door') {
    stimuli 'open', 'close'
    stimulus 'lock', { 'passcode' => :integer }
    stimulus 'unlock', { 'passcode' => :integer }
    responses 'opened', 'closed', 'locked', 'unlocked', 'invalid_command', 'invalid_passcode', 'incorrect_passcode', 'shut_off'
  }

  var 'selected_passcode', :integer, 0
  var 'incorrect_count', :integer, 0

  # Describe the behavior of this process. Statements are read top to bottom,
  # similar to an imperative program.

  # Door always starts closed and unlocked.
  state 'door_closed_unlocked'
  repeat {
    o {receive 'open'; send 'opened'; goto 'door_opened'}
    o {receive 'close'; send 'invalid_command'}
    o {
      receive 'lock',
        constraint: 'passcode >= 0 && passcode <= 9999',
        update: 'selected_passcode = passcode'
      send 'locked'
      goto 'door_closed_locked'
    }
    o {
      receive 'lock',
          constraint: 'passcode < 0 || passcode > 9999'
      send 'invalid_passcode'
    }
    o {
      receive 'unlock',
        constraint: 'passcode >= 0 && passcode <= 9999'
      send 'invalid_command'
    }
  }

  state 'door_opened'
  repeat {
    o {receive 'open'; send 'invalid_command'}
    o {receive 'close'; send 'closed'; goto 'door_closed_unlocked'}
    o {
      receive 'lock',
        constraint: 'passcode == true'
      send 'invalid_command'
    }
    o {
      receive 'unlock',
        constraint: 'passcode >= 0 && passcode <= 9999'
      send 'invalid_command'
    }
  }

  state 'door_closed_locked'
  repeat {
    o {receive 'open'; send 'invalid_command'}
    o {receive 'close'; send 'invalid_command'}
    o {
      receive 'lock',
        constraint: 'passcode >= 0'
      send 'invalid_command'
    }
    o {
      receive 'unlock',
        constraint: 'passcode == selected_passcode',
        update: 'incorrect_count = 0'
      send 'unlocked'
      goto 'door_closed_unlocked'}
    o {
      receive 'unlock',
          constraint: 'passcode < 0 || passcode > 9999'
      send 'invalid_passcode'
      }
    o {
        receive 'unlock',
          constraint: 'passcode != selected_passcode && incorrect_count >= 2'
        send 'incorrect_passcode'
        send 'shut_off'
        stop_repetition
    }
    o {
      receive 'unlock',
        constraint: 'passcode != selected_passcode && incorrect_count < 2',
        update: 'incorrect_count += 1'
      send 'incorrect_passcode'
    }
  }
}