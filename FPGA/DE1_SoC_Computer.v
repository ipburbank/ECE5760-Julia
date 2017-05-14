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

   wire                                            reset;
   assign reset = ~KEY[0];

   wire                                            CLOCK_100, CLOCK_SOLVER;

   //=======================================================
   //  Program Memory
   //=======================================================

   reg [7:0]                                       program_memory_address;
   wire [127:0]                                    vliw_instruction_broadcast_padded;
   wire [120:0]                                    vliw_instruction_broadcast = vliw_instruction_broadcast_padded[120:0];

   always @(posedge CLOCK_SOLVER) begin
      if (reset || (program_memory_address == 5)) program_memory_address <= 0;
      else program_memory_address <= program_memory_address + 1;
   end

   //=======================================================
   //  Mandelbrot Solver
   //=======================================================

   wire [26:0]                                     C_A_reference, C_A_reference_num, C_A_step, C_A_step_num, C_B, C_B_int;
   wire [9:0]                                      C_A_column;

   wire [26:0]                                     init_x, step, init_y;
   wire [9:0]                                      max_iterations;

   wire [9:0]                                      mandelbrot_pixel_x;
   wire [8:0]                                      mandelbrot_pixel_y;
   wire [7:0]                                      mandelbrot_pixel_color;
   wire                                            mandelbrot_pixel_stb;
   wire                                            frame_done_stb;

   Frame_Solver mandelbrot_solver (
                                   .solver_clk         (CLOCK_SOLVER),
                                   .reset              (reset),
                                   .x_0                (init_x),
                                   .x_step             (step),
                                   .y_0                (init_y),
                                   .y_step             (step),
                                   .max_iterations     (max_iterations),
                                   .vliw_instruction_broadcast (vliw_instruction_broadcast),
                                   .instruction_number (program_memory_address),
                                   .output_clk         (CLOCK_100),
                                   .output_pixel_x     (mandelbrot_pixel_x),
                                   .output_pixel_y     (mandelbrot_pixel_y),
                                   .output_pixel_color (mandelbrot_pixel_color),
                                   .output_pixel_stb   (mandelbrot_pixel_stb),
                                   .frame_done_stb     (frame_done_stb) // on solver clk
                                   );

   // ---------- time the frame generation ----------

   reg [15:0]                                      clock_50_to_ms_timer;
   reg [31:0]                                       frame_time_ms_accum, frame_time_ms;

   always @(posedge CLOCK_SOLVER) begin
      clock_50_to_ms_timer <= clock_50_to_ms_timer + 1;
      if (clock_50_to_ms_timer > 60000) begin
         clock_50_to_ms_timer <= 0;
         frame_time_ms_accum = frame_time_ms_accum + 1;
      end

      if (frame_done_stb) begin
         frame_time_ms <= frame_time_ms_accum;
         frame_time_ms_accum <= 0;
      end
   end

   //=======================================================
   // Controls for VGA memory
   //=======================================================
   wire [31:0]                                     vga_out_base_address = 32'h0000_0000;  // vga base addr
   wire [7:0]                                      vga_sram_writedata;
   wire [31:0]                                     vga_sram_address;
   wire                                            vga_sram_write;
   wire                                            vga_sram_clken = 1'b1;
   wire                                            vga_sram_chipselect = 1'b1;

   //=======================================================
   // pixel address is

   assign vga_sram_address = vga_out_base_address + {22'b0, mandelbrot_pixel_x} + ({23'b0,mandelbrot_pixel_y}*640);
   assign vga_sram_writedata = mandelbrot_pixel_color;
   assign vga_sram_write = mandelbrot_pixel_stb;

   // reg [2:0]                                       state;
   // always @(posedge CLOCK_50) begin // CLOCK_50

   //    // reset state machine and read/write controls
   //    if (reset) begin
   //       state <= 0 ;
   //       vga_sram_write <= 1'b0 ; // set to on if a write operation to bus
   //    end

   //    // --------------------------------------
   //    // write to the VGA sram
   //    if (state==8'd19) begin
   //       vga_sram_write <= 1'b1;
   //       // compute address
   //       vga_sram_address <= vga_out_base_address + {22'b0, mandelbrot_pixel_x} + ({22'b0,mandelbrot_pixel_y}*640);
   //       // data
   //       vga_sram_writedata <= mandelbrot_pixel_color;
   //    end
   // end // always @(posedge state_clock)


   //=======================================================
   //  Structural coding
   //=======================================================
   // From Qsys

   Computer_System The_System (
                               ////////////////////////////////////
                               // FPGA Side
                               ////////////////////////////////////

                               // Global signals
                               .system_pll_ref_clk_clk                                 (CLOCK_50),
                               .system_pll_ref_reset_reset                     (1'b0),

                               // // SRAM shared block with HPS
                               // .onchip_sram_s1_address               (sram_address),
                               // .onchip_sram_s1_clken                 (sram_clken),
                               // .onchip_sram_s1_chipselect            (sram_chipselect),
                               // .onchip_sram_s1_write                 (sram_write),
                               // .onchip_sram_s1_readdata              (sram_readdata),
                               // .onchip_sram_s1_writedata             (sram_writedata),
                               // .onchip_sram_s1_byteenable            (4'b1111),

                               //  sram to video
                               .onchip_vga_buffer_s1_address    (vga_sram_address),
                               .onchip_vga_buffer_s1_clken      (vga_sram_clken),
                               .onchip_vga_buffer_s1_chipselect (vga_sram_chipselect),
                               .onchip_vga_buffer_s1_write      (vga_sram_write),
                               .onchip_vga_buffer_s1_readdata   (),   // never read from vga here
                               .onchip_vga_buffer_s1_writedata  (vga_sram_writedata),

                               // AV Config
                               .av_config_SCLK                                                 (FPGA_I2C_SCLK),
                               .av_config_SDAT                                                 (FPGA_I2C_SDAT),

                               // 50 MHz clock bridge
                               .clock_bridge_0_in_clk_clk            (CLOCK_100), //(CLOCK_50),

                               // VGA Subsystem
                               .vga_pll_ref_clk_clk                                    (CLOCK2_50),
                               .vga_pll_ref_reset_reset                                (1'b0),
                               .vga_CLK                                                                                (VGA_CLK),
                               .vga_BLANK                                                                      (VGA_BLANK_N),
                               .vga_SYNC                                                                       (VGA_SYNC_N),
                               .vga_HS                                                                         (VGA_HS),
                               .vga_VS                                                                         (VGA_VS),
                               .vga_R                                                                          (VGA_R),
                               .vga_G                                                                          (VGA_G),
                               .vga_B                                                                          (VGA_B),

                               // // SDRAM
                               // .sdram_clk_clk                                                          (DRAM_CLK),
                               // .sdram_addr                                                                  (DRAM_ADDR),
                               // .sdram_ba                                                                       (DRAM_BA),
                               // .sdram_cas_n                                                            (DRAM_CAS_N),
                               // .sdram_cke                                                                      (DRAM_CKE),
                               // .sdram_cs_n                                                                     (DRAM_CS_N),
                               // .sdram_dq                                                                       (DRAM_DQ),
                               // .sdram_dqm                                                                      ({DRAM_UDQM,DRAM_LDQM}),
                               // .sdram_ras_n                                                            (DRAM_RAS_N),
                               // .sdram_we_n                                                                     (DRAM_WE_N),

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

                               .init_x_export                   (init_x),
                               .step_export                     (step),
                               .init_y_export                   (init_y),
                               .num_iter_export                 (max_iterations),
                               .frame_ms_export                 (frame_time_ms),

                               .clk_100mhz_clk                  (CLOCK_100),
                               .clk_solver_clk                  (CLOCK_SOLVER),

                               // instruction memory
                               .program_mem_clk_bridge_clk      (CLOCK_SOLVER),
                               .program_memory_address          (program_memory_address),
                               .program_memory_chipselect       (1'b1),
                               .program_memory_clken            (1'b1),
                               .program_memory_write            (1'b0),
                               .program_memory_readdata         (vliw_instruction_broadcast_padded),
                               .program_memory_writedata        (),//we don't write
                               .program_memory_byteenable       (-1)
                               );
endmodule // end top level
