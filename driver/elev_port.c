#include "elev.h"
#include "elev_port.h"
#include "erl_communication.h"


int main() 
{


  while (1) {
      byte command_buffer[MAX_COMMAND_SIZE];
      byte result[1];
      
      if(!wait_for_data(400))
      {
	  elev_set_motor_direction(0);
	  return 0;
      }
      

      if(read_cmd(command_buffer) > 0){
	  
	  byte command = command_buffer[0];
	  switch(command){
	  case(INIT_COMMAND):
	      elev_init(command_buffer[1]);
	      result[0] = 0;
	      break;
	  case(SET_MOTOR_DIRECTION_COMMAND):
	      elev_set_motor_direction(command_buffer[1]);
	      result[0] = 0;
	      break;
	  case(SET_DOOR_OPEN_LAMP_COMMAND):
	      elev_set_door_open_lamp(command_buffer[1]);
	      result[0] = 0;
	      break;
	  case(GET_OBSTRUCTION_SIGNAL_COMMAND):
	      result[0] = elev_get_obstruction_signal();
	      break;
	  case(GET_STOP_SIGNAL_COMMAND):
	      result[0] = elev_get_stop_signal();
	      break;
	  case(SET_STOP_LAMP_COMMAND):
	      elev_set_stop_lamp(command_buffer[1]);
	      result[0] = 0;
	      break;
	  case(GET_FLOOR_SENSOR_SIGNAL_COMMAND):
	      result[0] = elev_get_floor_sensor_signal();
	      break;
	  case(SET_FLOOR_INDICATION_COMMAND):
	      elev_set_floor_indicator(command_buffer[1]);
	      result[0] = 0;
	      break;
	  case(GET_BUTTON_SIGNAL_COMMAND):
	      result[0] = elev_get_button_signal(command_buffer[1], command_buffer[2]);
	      break;
	  case(SET_BUTTON_LAMP_COMMAND):
	      elev_set_button_lamp(command_buffer[1], command_buffer[2], command_buffer[3]);
	      result[0] = 0;
	      break;
	  }
	  write_cmd(result, 1);
      }
      

  }

  return 0;
}
