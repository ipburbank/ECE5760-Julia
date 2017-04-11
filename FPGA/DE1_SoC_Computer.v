module DE1_SoC_Computer (
                         ////////////////////////////////////
                         // FPGA Pins
                         ////////////////////////////////////

                         // Clock pins
                         CLOCK_50,
                         CLOCK2_50,
                         CLOCK3_50,
                         CLOCK4_50,

                         // ADC
                         ADC_CS_N,
                         ADC_DIN,
                         ADC_DOUT,
                         ADC_SCLK,

                         // Audio
                         AUD_ADCDAT,
                         AUD_ADCLRCK,
                         AUD_BCLK,
                         AUD_DACDAT,
                         AUD_DACLRCK,
                         AUD_XCK,

                         // SDRAM
                         DRAM_ADDR,
                         DRAM_BA,
                         DRAM_CAS_N,
                         DRAM_CKE,
                         DRAM_CLK,
                         DRAM_CS_N,
                         DRAM_DQ,
                         DRAM_LDQM,
                         DRAM_RAS_N,
                         DRAM_UDQM,
                         DRAM_WE_N,

                         // I2C Bus for Configuration of the Audio and Video-In Chips
                         FPGA_I2C_SCLK,
                         FPGA_I2C_SDAT,

                         // 40-Pin Headers
                         GPIO_0,
                         GPIO_1,

                         // Seven Segment Displays
                         HEX0,
                         HEX1,
                         HEX2,
                         HEX3,
                         HEX4,
                         HEX5,

                         // IR
                         IRDA_RXD,
                         IRDA_TXD,

                         // Pushbuttons
                         KEY,

                         // LEDs
                         LEDR,

                         // PS2 Ports
                         PS2_CLK,
                         PS2_DAT,

                         PS2_CLK2,
                         PS2_DAT2,

                         // Slider Switches
                         SW,

                         // Video-In
                         TD_CLK27,
                         TD_DATA,
                         TD_HS,
                         TD_RESET_N,
                         TD_VS,

                         // VGA
                         VGA_B,
                         VGA_BLANK_N,
                         VGA_CLK,
                         VGA_G,
                         VGA_HS,
                         VGA_R,
                         VGA_SYNC_N,
                         VGA_VS,

                         ////////////////////////////////////
                         // HPS Pins
                         ////////////////////////////////////

                         // DDR3 SDRAM
                         HPS_DDR3_ADDR,
                         HPS_DDR3_BA,
                         HPS_DDR3_CAS_N,
                         HPS_DDR3_CKE,
                         HPS_DDR3_CK_N,
                         HPS_DDR3_CK_P,
                         HPS_DDR3_CS_N,
                         HPS_DDR3_DM,
                         HPS_DDR3_DQ,
                         HPS_DDR3_DQS_N,
                         HPS_DDR3_DQS_P,
                         HPS_DDR3_ODT,
                         HPS_DDR3_RAS_N,
                         HPS_DDR3_RESET_N,
                         HPS_DDR3_RZQ,
                         HPS_DDR3_WE_N,

                         // Ethernet
                         HPS_ENET_GTX_CLK,
                         HPS_ENET_INT_N,
                         HPS_ENET_MDC,
                         HPS_ENET_MDIO,
                         HPS_ENET_RX_CLK,
                         HPS_ENET_RX_DATA,
                         HPS_ENET_RX_DV,
                         HPS_ENET_TX_DATA,
                         HPS_ENET_TX_EN,

                         // Flash
                         HPS_FLASH_DATA,
                         HPS_FLASH_DCLK,
                         HPS_FLASH_NCSO,

                         // Accelerometer
                         HPS_GSENSOR_INT,

                         // General Purpose I/O
                         HPS_GPIO,

                         // I2C
                         HPS_I2C_CONTROL,
                         HPS_I2C1_SCLK,
                         HPS_I2C1_SDAT,
                         HPS_I2C2_SCLK,
                         HPS_I2C2_SDAT,

                         // Pushbutton
                         HPS_KEY,

                         // LED
                         HPS_LED,

                         // SD Card
                         HPS_SD_CLK,
                         HPS_SD_CMD,
                         HPS_SD_DATA,

                         // SPI
                         HPS_SPIM_CLK,
                         HPS_SPIM_MISO,
                         HPS_SPIM_MOSI,
                         HPS_SPIM_SS,

                         // UART
                         HPS_UART_RX,
                         HPS_UART_TX,

                         // USB
                         HPS_CONV_USB_N,
                         HPS_USB_CLKOUT,
                         HPS_USB_DATA,
                         HPS_USB_DIR,
                         HPS_USB_NXT,
                         HPS_USB_STP
                         );

   //=======================================================
   //  PARAMETER declarations
   //=======================================================


   //=======================================================
   //  PORT declarations
   //=======================================================

   ////////////////////////////////////
   // FPGA Pins
   ////////////////////////////////////

   // Clock pins
   input                                           CLOCK_50;
   input                                           CLOCK2_50;
   input                                           CLOCK3_50;
   input                                           CLOCK4_50;

   // ADC
   inout                                           ADC_CS_N;
   output                                          ADC_DIN;
   input                                           ADC_DOUT;
   output                                          ADC_SCLK;

   // Audio
   input                                           AUD_ADCDAT;
   inout                                           AUD_ADCLRCK;
   inout                                           AUD_BCLK;
   output                                          AUD_DACDAT;
   inout                                           AUD_DACLRCK;
   output                                          AUD_XCK;

   // SDRAM
   output [12: 0]                                  DRAM_ADDR;
   output [ 1: 0]                                  DRAM_BA;
   output                                          DRAM_CAS_N;
   output                                          DRAM_CKE;
   output                                          DRAM_CLK;
   output                                          DRAM_CS_N;
   inout [15: 0]                                   DRAM_DQ;
   output                                          DRAM_LDQM;
   output                                          DRAM_RAS_N;
   output                                          DRAM_UDQM;
   output                                          DRAM_WE_N;

   // I2C Bus for Configuration of the Audio and Video-In Chips
   output                                          FPGA_I2C_SCLK;
   inout                                           FPGA_I2C_SDAT;

   // 40-pin headers
   inout [35: 0]                                   GPIO_0;
   inout [35: 0]                                   GPIO_1;

   // Seven Segment Displays
   output [ 6: 0]                                  HEX0;
   output [ 6: 0]                                  HEX1;
   output [ 6: 0]                                  HEX2;
   output [ 6: 0]                                  HEX3;
   output [ 6: 0]                                  HEX4;
   output [ 6: 0]                                  HEX5;

   // IR
   input                                           IRDA_RXD;
   output                                          IRDA_TXD;

   // Pushbuttons
   input [ 3: 0]                                   KEY;

   // LEDs
   output [ 9: 0]                                  LEDR;

   // PS2 Ports
   inout                                           PS2_CLK;
   inout                                           PS2_DAT;

   inout                                           PS2_CLK2;
   inout                                           PS2_DAT2;

   // Slider Switches
   input [ 9: 0]                                   SW;

   // Video-In
   input                                           TD_CLK27;
   input [ 7: 0]                                   TD_DATA;
   input                                           TD_HS;
   output                                          TD_RESET_N;
   input                                           TD_VS;

   // VGA
   output [ 7: 0]                                  VGA_B;
   output                                          VGA_BLANK_N;
   output                                          VGA_CLK;
   output [ 7: 0]                                  VGA_G;
   output                                          VGA_HS;
   output [ 7: 0]                                  VGA_R;
   output                                          VGA_SYNC_N;
   output                                          VGA_VS;



   ////////////////////////////////////
   // HPS Pins
   ////////////////////////////////////

   // DDR3 SDRAM
   output [14: 0]                                  HPS_DDR3_ADDR;
   output [ 2: 0]                                  HPS_DDR3_BA;
   output                                          HPS_DDR3_CAS_N;
   output                                          HPS_DDR3_CKE;
   output                                          HPS_DDR3_CK_N;
   output                                          HPS_DDR3_CK_P;
   output                                          HPS_DDR3_CS_N;
   output [ 3: 0]                                  HPS_DDR3_DM;
   inout [31: 0]                                   HPS_DDR3_DQ;
   inout [ 3: 0]                                   HPS_DDR3_DQS_N;
   inout [ 3: 0]                                   HPS_DDR3_DQS_P;
   output                                          HPS_DDR3_ODT;
   output                                          HPS_DDR3_RAS_N;
   output                                          HPS_DDR3_RESET_N;
   input                                           HPS_DDR3_RZQ;
   output                                          HPS_DDR3_WE_N;

   // Ethernet
   output                                          HPS_ENET_GTX_CLK;
   inout                                           HPS_ENET_INT_N;
   output                                          HPS_ENET_MDC;
   inout                                           HPS_ENET_MDIO;
   input                                           HPS_ENET_RX_CLK;
   input [ 3: 0]                                   HPS_ENET_RX_DATA;
   input                                           HPS_ENET_RX_DV;
   output [ 3: 0]                                  HPS_ENET_TX_DATA;
   output                                          HPS_ENET_TX_EN;

   // Flash
   inout [ 3: 0]                                   HPS_FLASH_DATA;
   output                                          HPS_FLASH_DCLK;
   output                                          HPS_FLASH_NCSO;

   // Accelerometer
   inout                                           HPS_GSENSOR_INT;

   // General Purpose I/O
   inout [ 1: 0]                                   HPS_GPIO;

   // I2C
   inout                                           HPS_I2C_CONTROL;
   inout                                           HPS_I2C1_SCLK;
   inout                                           HPS_I2C1_SDAT;
   inout                                           HPS_I2C2_SCLK;
   inout                                           HPS_I2C2_SDAT;

   // Pushbutton
   inout                                           HPS_KEY;

   // LED
   inout                                           HPS_LED;

   // SD Card
   output                                          HPS_SD_CLK;
   inout                                           HPS_SD_CMD;
   inout [ 3: 0]                                   HPS_SD_DATA;

   // SPI
   output                                          HPS_SPIM_CLK;
   input                                           HPS_SPIM_MISO;
   output                                          HPS_SPIM_MOSI;
   inout                                           HPS_SPIM_SS;

   // UART
   input                                           HPS_UART_RX;
   output                                          HPS_UART_TX;

   // USB
   inout                                           HPS_CONV_USB_N;
   input                                           HPS_USB_CLKOUT;
   inout [ 7: 0]                                   HPS_USB_DATA;
   input                                           HPS_USB_DIR;
   input                                           HPS_USB_NXT;
   output                                          HPS_USB_STP;

   //=======================================================
   //  REG/WIRE declarations
   //=======================================================

   wire [15: 0]                                    hex3_hex0;
   //wire                  [15: 0] hex5_hex4;

   //assign HEX0 = ~hex3_hex0[ 6: 0]; // hex3_hex0[ 6: 0];
   //assign HEX1 = ~hex3_hex0[14: 8];
   //assign HEX2 = ~hex3_hex0[22:16];
   //assign HEX3 = ~hex3_hex0[30:24];
   //assign HEX4 = 7'b1111111;
   //assign HEX5 = 7'b1111111;

   wire                                            reset;
   assign reset = ~KEY[0];

   ////////////////////////////////////////////////////////////////////
   // Framebuffer
   ////////////////////////////////////////////////////////////////////
   localparam NUM_ROWS = 480;

   wire [7:0]                                      framebuffer_read_data [0:NUM_ROWS-1];
   wire [7:0]                                      framebuffer_write_data [0:NUM_ROWS-1];
   wire [9:0]                                      framebuffer_read_addr; // read from them all, mux only the one we care about
   wire [9:0]                                      framebuffer_write_addr [0:NUM_ROWS-1];
   wire                                            framebuffer_wren [0:NUM_ROWS-1];

   wire                                            VGA_CLK;

   genvar                                          row_ram_i;
   generate
      for(row_ram_i=0; row_ram_i < NUM_ROWS; row_ram_i=row_ram_i+1) begin : row_rams
         Row_RAM ram(
                     .rdaddress (framebuffer_read_addr),
                     .q         (framebuffer_read_data[row_ram_i]),
                     .rdclock   (VGA_CLK),
                     .wraddress (framebuffer_write_addr[row_ram_i]),
                     .wrclock   (CLOCK_50),
                     .wren      (framebuffer_wren[row_ram_i]),
                     .data      (framebuffer_write_data[row_ram_i]),
                     );

      end
   endgenerate


   ////////////////////////////////////////////////////////////////////
   // Pixel generation state machine
   ////////////////////////////////////////////////////////////////////
   localparam NUM_ROWS_SOLVERS = 10;

   wire [26:0] x_0, x_step, y_0, y_step;

   wire [NUM_ROWS_SOLVERS-1:0]                     row_solver_start_request,
                                                   row_solver_start_grant,
                                                   row_solver_done;

   wire [9:0]                                      row_solver_results [NUM_ROWS_SOLVERS-1:0];

   Reqs_To_One_Hot #(NUM_ROWS_SOLVERS) (
                                        .reqs(row_solver_start_request),
                                        .grants(row_solver_start_grant)
                                        );

   reg [9:0]                                       row_next_y_idx;
   reg [26:0]                                      row_next_y_value;
   wire [26:0]                                     row_next_y_value_adder_out;
   always @(posedge CLOCK_50) begin
      if (reset) begin
         row_next_y_idx <= 0;
         row_next_y_value <= y_0;
      end
      else if (row_solver_start_grant > 0) begin
         row_next_y_idx <= row_next_y_idx + 1;
         row_next_y_value <= row_next_y_value_adder_out;
      end
   end

   FpAdd FpAdder(CLOCK_50, row_next_y_value, y_step, row_next_y_value_adder_out);

   genvar solver_i;
   generate
      for(solver_i=0; solver_i < NUM_ROWS_SOLVERS; solver_i=solver_i+1) begin : row_solvers

         always @(posedge CLOCK_50) begin

         end

         Row_Solver solver(
                           .solver_clk      (CLOCK_50),
                           .reset           (reset),
                           .start_request   (row_solver_start_request[solver_i]),
                           .start_grant     (row_solver_start_grant[solver_i]),
                           .row_x_reference (x_0),
                           .row_x_step      (x_step),
                             .row_y           (row_next_y_value),
                           .output_value    (row_solver_results[solver_i]),
                           .output_stb      (row_solver_done[solver_i])
                           );
      end
   endgenerate

   ////////////////////////////////////////////////////////////////////
   // VGA machine
   ////////////////////////////////////////////////////////////////////

   wire CLOCK_27;
   wire	VGA_CTRL_CLK;
   wire	AUD_CTRL_CLK;
   wire	DLY_RST;

   assign	TD_RESET_N	=	1'b1;	//	Allow 27 MHz
   assign	AUD_ADCLRCK	=	AUD_DACLRCK;
   assign	AUD_XCK		=	AUD_CTRL_CLK;

   VGA_Audio_PLL p1(.areset(~reset),.inclk0(CLOCK_27),.c0(VGA_CTRL_CLK),.c1(AUD_CTRL_CLK),.c2(VGA_CLK));

   wire [9:0] vga_ctrl_x_coord, vga_ctrl_y_coord;
   wire [9:0] vga_ctrl_r, vga_ctrl_g, vga_ctrl_b;
   wire [19:0] mVGA_ADDR;

   assign framebuffer_read_addr = vga_ctrl_y_coord;

   assign vga_ctrl_r = framebuffer_read_data[vga_ctrl_x_coord][7:6]; // TODO delay x cood


   VGA_Controller vga_controller (
                                  // Host Side
                                  .iCursor_RGB_EN(4'b0111),
                                  .oAddress(mVGA_ADDR),
                                  .oCoord_X(vga_ctrl_x_coord),
                                  .oCoord_Y(vga_ctrl_y_coord),
                                  .iRed(vga_ctrl_r),
                                  .iGreen(vga_ctrl_g),
                                  .iBlue(vga_ctrl_b),
                                  // VGA Side
                                  .oVGA_R(VGA_R),
                                  .oVGA_G(VGA_G),
                                  .oVGA_B(VGA_B),
                                  .oVGA_H_SYNC(VGA_HS),
                                  .oVGA_V_SYNC(VGA_VS),
                                  .oVGA_SYNC(VGA_SYNC_N),
                                  .oVGA_BLANK(VGA_BLANK_N),
                                  // control Signal
                                  .iCLK(VGA_CTRL_CLK),
                                  .iRST_N(reset)
                                  );


   //=======================================================
   //  Structural coding
   //=======================================================

   Computer_System The_System (
                               ////////////////////////////////////
                               // FPGA Side
                               ////////////////////////////////////

                               // Global signals
                               .system_pll_ref_clk_clk                                 (CLOCK_50), //?
                               .system_pll_ref_reset_reset                     (1'b0),

                               ////////////////////////////////////
                               // HPS Side
                               ////////////////////////////////////
                               // DDR3 SDRAM
                               .memory_mem_a                   (HPS_DDR3_ADDR),
                               .memory_mem_ba                  (HPS_DDR3_BA),
                               .memory_mem_ck                  (HPS_DDR3_CK_P),
                               .memory_mem_ck_n                (HPS_DDR3_CK_N),
                               .memory_mem_cke         (HPS_DDR3_CKE),
                               .memory_mem_cs_n                (HPS_DDR3_CS_N),
                               .memory_mem_ras_n               (HPS_DDR3_RAS_N),
                               .memory_mem_cas_n               (HPS_DDR3_CAS_N),
                               .memory_mem_we_n                (HPS_DDR3_WE_N),
                               .memory_mem_reset_n     (HPS_DDR3_RESET_N),
                               .memory_mem_dq                  (HPS_DDR3_DQ),
                               .memory_mem_dqs         (HPS_DDR3_DQS_P),
                               .memory_mem_dqs_n               (HPS_DDR3_DQS_N),
                               .memory_mem_odt         (HPS_DDR3_ODT),
                               .memory_mem_dm                  (HPS_DDR3_DM),
                               .memory_oct_rzqin               (HPS_DDR3_RZQ),

                               // Ethernet
                               .hps_io_hps_io_gpio_inst_GPIO35 (HPS_ENET_INT_N),
                               .hps_io_hps_io_emac1_inst_TX_CLK        (HPS_ENET_GTX_CLK),
                               .hps_io_hps_io_emac1_inst_TXD0  (HPS_ENET_TX_DATA[0]),
                               .hps_io_hps_io_emac1_inst_TXD1  (HPS_ENET_TX_DATA[1]),
                               .hps_io_hps_io_emac1_inst_TXD2  (HPS_ENET_TX_DATA[2]),
                               .hps_io_hps_io_emac1_inst_TXD3  (HPS_ENET_TX_DATA[3]),
                               .hps_io_hps_io_emac1_inst_RXD0  (HPS_ENET_RX_DATA[0]),
                               .hps_io_hps_io_emac1_inst_MDIO  (HPS_ENET_MDIO),
                               .hps_io_hps_io_emac1_inst_MDC           (HPS_ENET_MDC),
                               .hps_io_hps_io_emac1_inst_RX_CTL        (HPS_ENET_RX_DV),
                               .hps_io_hps_io_emac1_inst_TX_CTL        (HPS_ENET_TX_EN),
                               .hps_io_hps_io_emac1_inst_RX_CLK        (HPS_ENET_RX_CLK),
                               .hps_io_hps_io_emac1_inst_RXD1  (HPS_ENET_RX_DATA[1]),
                               .hps_io_hps_io_emac1_inst_RXD2  (HPS_ENET_RX_DATA[2]),
                               .hps_io_hps_io_emac1_inst_RXD3  (HPS_ENET_RX_DATA[3]),

                               // Flash
                               .hps_io_hps_io_qspi_inst_IO0    (HPS_FLASH_DATA[0]),
                               .hps_io_hps_io_qspi_inst_IO1    (HPS_FLASH_DATA[1]),
                               .hps_io_hps_io_qspi_inst_IO2    (HPS_FLASH_DATA[2]),
                               .hps_io_hps_io_qspi_inst_IO3    (HPS_FLASH_DATA[3]),
                               .hps_io_hps_io_qspi_inst_SS0    (HPS_FLASH_NCSO),
                               .hps_io_hps_io_qspi_inst_CLK    (HPS_FLASH_DCLK),

                               // Accelerometer
                               .hps_io_hps_io_gpio_inst_GPIO61 (HPS_GSENSOR_INT),

                               //.adc_sclk                        (ADC_SCLK),
                               //.adc_cs_n                        (ADC_CS_N),
                               //.adc_dout                        (ADC_DOUT),
                               //.adc_din                         (ADC_DIN),

                               // General Purpose I/O
                               .hps_io_hps_io_gpio_inst_GPIO40 (HPS_GPIO[0]),
                               .hps_io_hps_io_gpio_inst_GPIO41 (HPS_GPIO[1]),

                               // I2C
                               .hps_io_hps_io_gpio_inst_GPIO48 (HPS_I2C_CONTROL),
                               .hps_io_hps_io_i2c0_inst_SDA            (HPS_I2C1_SDAT),
                               .hps_io_hps_io_i2c0_inst_SCL            (HPS_I2C1_SCLK),
                               .hps_io_hps_io_i2c1_inst_SDA            (HPS_I2C2_SDAT),
                               .hps_io_hps_io_i2c1_inst_SCL            (HPS_I2C2_SCLK),

                               // Pushbutton
                               .hps_io_hps_io_gpio_inst_GPIO54 (HPS_KEY),

                               // LED
                               .hps_io_hps_io_gpio_inst_GPIO53 (HPS_LED),

                               // SD Card
                               .hps_io_hps_io_sdio_inst_CMD    (HPS_SD_CMD),
                               .hps_io_hps_io_sdio_inst_D0     (HPS_SD_DATA[0]),
                               .hps_io_hps_io_sdio_inst_D1     (HPS_SD_DATA[1]),
                               .hps_io_hps_io_sdio_inst_CLK    (HPS_SD_CLK),
                               .hps_io_hps_io_sdio_inst_D2     (HPS_SD_DATA[2]),
                               .hps_io_hps_io_sdio_inst_D3     (HPS_SD_DATA[3]),

                               // SPI
                               .hps_io_hps_io_spim1_inst_CLK           (HPS_SPIM_CLK),
                               .hps_io_hps_io_spim1_inst_MOSI  (HPS_SPIM_MOSI),
                               .hps_io_hps_io_spim1_inst_MISO  (HPS_SPIM_MISO),
                               .hps_io_hps_io_spim1_inst_SS0           (HPS_SPIM_SS),

                               // UART
                               .hps_io_hps_io_uart0_inst_RX    (HPS_UART_RX),
                               .hps_io_hps_io_uart0_inst_TX    (HPS_UART_TX),

                               // USB
                               .hps_io_hps_io_gpio_inst_GPIO09 (HPS_CONV_USB_N),
                               .hps_io_hps_io_usb1_inst_D0             (HPS_USB_DATA[0]),
                               .hps_io_hps_io_usb1_inst_D1             (HPS_USB_DATA[1]),
                               .hps_io_hps_io_usb1_inst_D2             (HPS_USB_DATA[2]),
                               .hps_io_hps_io_usb1_inst_D3             (HPS_USB_DATA[3]),
                               .hps_io_hps_io_usb1_inst_D4             (HPS_USB_DATA[4]),
                               .hps_io_hps_io_usb1_inst_D5             (HPS_USB_DATA[5]),
                               .hps_io_hps_io_usb1_inst_D6             (HPS_USB_DATA[6]),
                               .hps_io_hps_io_usb1_inst_D7             (HPS_USB_DATA[7]),
                               .hps_io_hps_io_usb1_inst_CLK            (HPS_USB_CLKOUT),
                               .hps_io_hps_io_usb1_inst_STP            (HPS_USB_STP),
                               .hps_io_hps_io_usb1_inst_DIR            (HPS_USB_DIR),
                               .hps_io_hps_io_usb1_inst_NXT            (HPS_USB_NXT),

                               .x_0_export                      (x_0),
                               .x_step_export                   (x_step),
                               .y_0_export                      (y_0),
                               .y_step_export                   (y_step),
                               .clk_27_clk          (CLOCK_27)
                               );
endmodule
