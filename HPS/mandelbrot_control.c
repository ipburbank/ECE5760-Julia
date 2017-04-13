/**
 * @file   game_of_life.c
 * @author Istvan Burbank
 * @author Bruce Land
 * @date   2017-04-13
 * @brief  Mandelbrot Controller
 *
 */

/*******************************/
/* Includes                    */
/*******************************/
//@{

#include "address_map_arm.h"
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/mman.h>
#include <sys/shm.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdint.h>
#include <ctype.h>
#include <getopt.h>
#include <math.h>

//@}

/*******************************/
/* LOCAL Macro Definitions     */
/*******************************/
//@{
// Cursor size in pixels
#define SCREEN_WIDTH  (640)
#define SCREEN_HEIGHT (480)

// pixel macro
#define VGA_PIXEL(x,y,color) do{\
    char  *pixel_ptr ;                                          \
    pixel_ptr = (char *)vga_pixel_ptr + ((y)<<10) + (x) ;       \
    *(char *)pixel_ptr = (color);                               \
} while(0)

//@}

/********************************/
/* LOCAL Type(def) Declarations */
/********************************/
//@{

struct mouse_state{
  int  x;                   /**< Mouse x position */
  int  y;                   /**< Mouse y position  */
  bool left;                /**< Left mouse button state */
  bool middle;              /**< Middle mouse button state */
  bool right;               /**< Right mouse button state */
};

//@}

/*******************************/
/* LOCAL Variable Definitions  */
/*******************************/
//@{

// the light weight buss base
void *h2p_lw_virtual_base;

// pixel buffer
volatile unsigned int * vga_pixel_ptr = NULL ;
void *vga_pixel_virtual_base;

// character buffer
volatile unsigned int *vga_char_ptr = NULL;
void *vga_char_virtual_base;

// access to switches
volatile unsigned int *h2p_lw_sw_addr=NULL;

// access to pios
volatile unsigned int *h2p_x_0_addr=NULL;
volatile unsigned int *h2p_step_addr=NULL;
volatile unsigned int *h2p_y_0_addr=NULL;
volatile unsigned int *h2p_num_iter_addr=NULL;
volatile unsigned int *h2p_frame_ms_addr=NULL;

// /dev/mem file id
int mem_fd;

// shared memory
key_t mem_key = 0xf0;
int shared_mem_id;
int *shared_ptr;
int shared_time;
int shared_note;
char shared_str[64];

// measure time
struct timeval t1, t2;
double elapsedTime;

// mouse position
int mouse_fd;
struct mouse_state mouse_state;

//@}

/*******************************/
/* LOCAL Function Declarations */
/*******************************/
//@{


/**
 * @brief Display a string of text to the VGA monitor
 *
 * @param x        X-coordinate of the start of the text
 * @param y        Y-coordinate of the start of the text
 * @param text_ptr C string to display
 */
void VGA_text(int x, int y, char *text_ptr);
void VGA_text_clear();
void VGA_box(int, int, int, int, short);

/**
 * @brief Update the global `mouse_state` and redraw the mouse
 */
void mouse_update();

/**
 * @brief floating point stuff
 */
unsigned int floatToReg27(float f);
float reg27ToFloat(unsigned int r);

//@}

/*******************************/
/* GLOBAL Variable Definitions */
/*******************************/
//@{

//@}

/*******************************/
/* GLOBAL Function Definitions */
/*******************************/
//@{

int main(int argc, char *argv[]) {
  // int x1, y1, x2, y2;

  // Declare volatile pointers to I/O registers (volatile means that IO load and
  // store instructions will be used to access these pointer locations, instead
  // of regular memory loads and stores)

  // === shared memory =======================
  // with video process
  shared_mem_id = shmget(mem_key, 100, IPC_CREAT | 0666);
  // shared_mem_id = shmget(mem_key, 100, 0666);
  shared_ptr = shmat(shared_mem_id, NULL, 0);

  // === get FPGA addresses ==================
  // Open /dev/mem
  if ((mem_fd = open("/dev/mem", (O_RDWR | O_SYNC))) == -1) {
    printf("ERROR: could not open \"/dev/mem\"...\n");
    return (1);
  }

  // get virtual addr that maps to physical
  h2p_lw_virtual_base = mmap(NULL, HW_REGS_SPAN, (PROT_READ | PROT_WRITE),
                             MAP_SHARED, mem_fd, HW_REGS_BASE);
  if (h2p_lw_virtual_base == MAP_FAILED) {
    printf("ERROR: mmap1() failed...\n");
    close(mem_fd);
    return (1);
  }

  // Get the address that maps to the FPGA LED control
  h2p_lw_sw_addr = (unsigned int *)(h2p_lw_virtual_base + ((SW_BASE)));

  // === get VGA char addr =====================
  // get virtual addr that maps to physical
  vga_char_virtual_base = mmap(NULL, FPGA_CHAR_SPAN, (PROT_READ | PROT_WRITE),
                               MAP_SHARED, mem_fd, FPGA_CHAR_BASE);
  if (vga_char_virtual_base == MAP_FAILED) {
    printf("ERROR: mmap2() failed...\n");
    close(mem_fd);
    return (1);
  }

  // Get the address that maps to the FPGA LED control
  vga_char_ptr = (unsigned int *)(vga_char_virtual_base);

  // === get VGA pixel addr ====================
  // get virtual addr that maps to physical
  vga_pixel_virtual_base =
      mmap(NULL, FPGA_ONCHIP_SPAN, (PROT_READ | PROT_WRITE), MAP_SHARED, mem_fd,
           FPGA_ONCHIP_BASE);
  if (vga_pixel_virtual_base == MAP_FAILED) {
    printf("ERROR: mmap3() failed...\n");
    close(mem_fd);
    return (1);
  }

  // Get the address that maps to the FPGA pixel buffer
  vga_pixel_ptr =(unsigned int *)(vga_pixel_virtual_base);

  // ===========================================

  /* Configure the mouse
   */
  // set mouse to start in the middle of the screen by default
  mouse_state.x = 320;
  mouse_state.y = 240;

  const char *pDevice = "/dev/input/mice";

  // Open Mouse
  mouse_fd = open(pDevice, O_RDWR);
  if(mouse_fd == -1)
    {
      printf("ERROR Opening %s\n", pDevice);
      return -1;
    }

  // needed for nonblocking read()
  int flags = fcntl(mouse_fd, F_GETFL, 0);
  fcntl(mouse_fd, F_SETFL, flags | O_NONBLOCK);

  // === PIOs ==================================
  h2p_x_0_addr  = (unsigned int *)(h2p_lw_virtual_base + (PIO_X_0));
  h2p_step_addr = (unsigned int *)(h2p_lw_virtual_base + (PIO_STEP));
  h2p_y_0_addr  = (unsigned int *)(h2p_lw_virtual_base + (PIO_Y_0));
  h2p_num_iter_addr  = (unsigned int *)(h2p_lw_virtual_base + (PIO_NUM_ITER));
  h2p_frame_ms_addr  = (unsigned int *)(h2p_lw_virtual_base + (PIO_FRAME_MS));

  /* create a message to be displayed on the VGA
    and LCD displays */
  char text_top_row[40] = "DE1-SoC ARM/FPGA\0";
  char text_bottom_row[40] = "Cornell ece5760\0";
  char num_string[20], time_string[40];

  // clear the screen
  VGA_box(0, 0, 639, 479, 0x00);
  // clear the text
  VGA_text_clear();
  VGA_text(1, 1, text_top_row);
  VGA_text(1, 2, text_bottom_row);

  // =============================================
  // === PARSE INPUTS ============================
  static struct option long_options[] =
    {
      {"i",  required_argument, NULL, 'i'},
      {NULL,  0,                 NULL, 0}
    };

  int opt;
  int long_index =0;
  while ((opt = getopt_long_only(argc, argv, "", long_options, &long_index )) != -1) {
    int newval = (int)strtol(optarg, NULL, 16);
    switch (opt) {
    case 'i':
      *h2p_num_iter_addr = newval;
      break;
    default:
      printf("Invalid Option\n");
      exit(EXIT_FAILURE);
    }
  }

  mouse_state.x = 0;
  mouse_state.y = 0;
  VGA_text_clear();

  // set an initial position
  *h2p_x_0_addr  = floatToReg27(-2.0);
  *h2p_y_0_addr  = floatToReg27(-1.1);
  *h2p_step_addr = floatToReg27(3.0/640.0);

  // load position once to avoid rounding errors
  double step = reg27ToFloat(*h2p_step_addr);

  double x_position = reg27ToFloat(*h2p_x_0_addr);
  double y_position = reg27ToFloat(*h2p_y_0_addr);

  usleep(10000);
  while(1) {
    mouse_update();

    x_position += (double) mouse_state.x * 10.0 * step;
    mouse_state.x = 0;
    *h2p_x_0_addr = floatToReg27(x_position);

    y_position += (double) mouse_state.y * 10.0 * step;
    mouse_state.y = 0;
    *h2p_y_0_addr = floatToReg27(y_position);

    if(mouse_state.left){
      double center_x, center_y;
      center_x = x_position + 320.0*step;
      center_y = y_position + 240.0*step;

      step *= 1.003;

      *h2p_x_0_addr = floatToReg27(center_x - 320.0*step);
      *h2p_y_0_addr = floatToReg27(center_y - 240.0*step);

      *h2p_step_addr = floatToReg27(step);
    }

    if(mouse_state.right){
      double center_x, center_y;
      center_x = x_position + 320.0*step;
      center_y = y_position + 240.0*step;

      step /= 1.003;

      *h2p_x_0_addr = floatToReg27(center_x - 320.0*step);
      *h2p_y_0_addr = floatToReg27(center_y - 240.0*step);

      *h2p_step_addr = floatToReg27(step);
    }
  
    /* create a message to be displayed on the VGA
       and LCD displays */
    char text_top_row[40];
    char text_bottom_row[40] = "Cornell ece5760\0";

    // write the text to the buffers
    sprintf(text_top_row, "Time: %ims\0", *h2p_frame_ms_addr);
    sprintf(text_bottom_row, "(%.5f, %.5f),(%.5f, %.5f)", x_position, y_position, x_position+640*step, y_position+480*step);

    usleep(10000);

    // draws the text
    VGA_text_clear();
    VGA_text(1, 1, text_top_row);
    VGA_text(1, 2, text_bottom_row);
  }

  return 0;
} // end main

//@}

/*******************************/
/* LOCAL Function Definitions  */
/*******************************/
//@{

void VGA_text(int x, int y, char *text_ptr) {
  volatile char *character_buffer =
      (char *)vga_char_ptr; // VGA character buffer
  int offset;
  /* assume that the text string fits on one line */
  offset = (y << 7) + x;
  while (*(text_ptr)) {
    // write to the character buffer
    *(character_buffer + offset) = *(text_ptr);
    ++text_ptr;
    ++offset;
  }
}

/****************************************************************************************
 * Subroutine to clear text to the VGA monitor
****************************************************************************************/
void VGA_text_clear() {
  volatile char *character_buffer =
      (char *)vga_char_ptr; // VGA character buffer
  int offset, x, y;
  for (x = 0; x < 70; x++) {
    for (y = 0; y < 40; y++) {
      /* assume that the text string fits on one line */
      offset = (y << 7) + x;
      // write to the character buffer
      *(character_buffer + offset) = ' ';
    }
  }
}

/****************************************************************************************
 * Draw a filled rectangle on the VGA monitor
****************************************************************************************/
#define SWAP(X, Y)                                                             \
  do {                                                                         \
    int temp = X;                                                              \
    X = Y;                                                                     \
    Y = temp;                                                                  \
  } while (0)

void VGA_box(int x1, int y1, int x2, int y2, short pixel_color) {
  char *pixel_ptr;
  int row, col;

  /* check and fix box coordinates to be valid */
  if (x1 > 639)
    x1 = 639;
  if (y1 > 479)
    y1 = 479;
  if (x2 > 639)
    x2 = 639;
  if (y2 > 479)
    y2 = 479;
  if (x1 < 0)
    x1 = 0;
  if (y1 < 0)
    y1 = 0;
  if (x2 < 0)
    x2 = 0;
  if (y2 < 0)
    y2 = 0;
  if (x1 > x2)
    SWAP(x1, x2);
  if (y1 > y2)
    SWAP(y1, y2);
  for (row = y1; row <= y2; row++)
    for (col = x1; col <= x2; ++col) {
      // 640x480
      pixel_ptr = (char *)vga_pixel_ptr + (row << 10) + col;
      // set pixel color
      *(char *)pixel_ptr = pixel_color;
    }
}

void mouse_update() {
  int8_t data[3];

  // update the mouse state variable
  int bytes = read(mouse_fd, data, sizeof(data));

  if (bytes > 0) {
    mouse_state.left   = !!(data[0] & 0x1);
    mouse_state.middle = !!(data[0] & 0x4);
    mouse_state.right  = !!(data[0] & 0x2);

    mouse_state.x = data[1];

    mouse_state.y = -1 * data[2]; // mouse up positive, display up negative
  }
}

/**************************************************************************
 * Mark Eiding mje56                                                      *
 * ECE 5760                                                               *
 * Modified IEEE single precision FP                                      *
 * bit 26:      Sign     (0: pos, 1: neg)                                 *
 * bits[25:18]: Exponent (unsigned)                                       *
 * bits[17:0]:  Fraction (unsigned)                                       *
 *  (-1)^SIGN * 2^(EXP-127) * (1+.FRAC)                                   *
 * (http://en.wikipedia.org/wiki/Single-precision_floating-point_format)  *
 * Adapted from Skyler Schneider ss868                                    *
 *************************************************************************/
// Convert a C floating point into a 27-bit register floating point.
unsigned int floatToReg27(float f) {
    int f_f = (*(int*)&f);
    int f_sign = (f_f >> 31) & 0x1;
    int f_exp = (f_f >> 23) & 0xFF;
    int f_frac = f_f & 0x007FFFFF;
    int r_sign;
    int r_exp;
    int r_frac;
    r_sign = f_sign;
    if((f_exp == 0x00) || (f_exp == 0xFF)) {
        // 0x00 -> 0 or subnormal
        // 0xFF -> infinity or NaN
        r_exp = 0;
        r_frac = 0;
    } else {
        r_exp = (f_exp) & 0xFF;
        r_frac = ((f_frac >> 5)) & 0x0003FFFF;
    }
    return (r_sign << 26) | (r_exp << 18) | r_frac;
}

// Convert a 27-bit register floating point into a C floating point.
float reg27ToFloat(unsigned int r) {
    int sign = (r & 0x04000000) >> 26;
    unsigned int exp = (r & 0x03FC0000) >> 18;
    int frac = (r & 0x0003FFFF);
    float result = pow(2.0, (float) (exp-127.0));
    result = (1.0+(((float)frac) / 262144.0)) * result;
    if(sign) result = result * (-1);
    return result;
}

//@}
