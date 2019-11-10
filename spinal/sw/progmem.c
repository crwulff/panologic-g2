#include <stdint.h>
#include <math.h>

#include "reg.h"
#include "top_defines.h"
#include "print.h"
#include "i2c.h"
#include "gmii.h"
#include "dvi.h"
#include "usb.h"

static inline uint32_t rdcycle(void) {
    uint32_t cycle;
    asm volatile ("rdcycle %0" : "=r"(cycle));
    return cycle;
}

static inline int nop(void) {
    asm volatile ("addi x0, x0, 0");
    return 0;
}

void wait(int cycles)
{
#if 1
    volatile int cnt = 0;

    for(int i=0;i<cycles;++i){
        ++cnt;
    }
#else
    int start;

    start = rdcycle();
    while ((rdcycle() - start) <= cycles);
#endif
}

void DDR_memtest(void);


#define WAIT_CYCLES 500000

int button_pressed()
{
    return (REG_RD(LED_READ) & 0x04) == 0x00;
}

int main() {

    REG_WR(LED_DIR, 0xff);

    dvi_ctrl_init();

#if 0
    while(1){
        REG_WR(LED_WRITE, 0x01);
        wait(WAIT_CYCLES);
        REG_WR(LED_WRITE, 0x02);
        wait(WAIT_CYCLES);
    }
#endif

#if 1
    clear();
    print("Pano Logic G2 Reverse Engineering\n");
    print("Compiled " __DATE__ " " __TIME__ "\n\n");
    print("---------------------------------\n");
    print("\n");
    print("Spartan-6 LX150 FPGA\n");
    print("DVI & HDMI working @ 1080p\n");
    print("\n");
    print("Code at github.com/tomverbeure/panologic-g2\n");
#endif

#if 0
    // Basis ULPI bus monitoring.
    ulpi_print_id();
    ulpi_reset_bus();
    ulpi_monitor_rx_cmd();      // This is an endless loop
#endif

#if 0
    // Basic test that dumps received packets on the GMII interface
    gmii_mdio_init();
    //gmii_reg_dump(0);
    //gmii_print_phy_id(0);
    gmii_wait_auto_neg_complete(0);
    //gmii_reg_dump(0);

    gmii_dump_packets(0);
#endif
#if 0
    // SPI Flash test (read first 256 bytes)
    uint32_t address = 0x00000000;
    *(volatile uint32_t*)0x80000708 = 0x00000000; // config
    *(volatile uint32_t*)0x8000070C = 0x00000000; // clockDivider = 1 (25MHz / 2)
    *(volatile uint32_t*)0x80000710 = 0x00000001; // ssSetup time
    *(volatile uint32_t*)0x80000714 = 0x00000001; // ssHold time
    *(volatile uint32_t*)0x80000718 = 0x00000001; // ssDisable time

    do {
        *(volatile uint32_t*)0x80000700 = 0x11000000; // SS Enable
        *(volatile uint32_t*)0x80000700 = 0x00000003; // READ DATA BYTES
        *(volatile uint32_t*)0x80000700 = (address >> 16) & 0xFF; // A[23-16]
        *(volatile uint32_t*)0x80000700 = (address >> 8) & 0xFF; // A[15-8]
        *(volatile uint32_t*)0x80000700 = address & 0xFF; // A[7-0]

        for (int i=0; i<16; i++) {
            *(volatile uint32_t*)0x80000700 = 0x01000000; // Read Data
   }

        *(volatile uint32_t*)0x80000700 = 0x10000000; // SS Disable

        print("SPI Flash: ");
        for (int i=0; i<16; i++) {
            uint32_t spiRd = 0;
            while (((spiRd = *(volatile uint32_t*)0x80000700) & 0x80000000) == 0) {}
            print_byte(spiRd & 0xFF, 1);
            print(" ");
        }
        print("\n");
        address += 16;
    } while (address < 256);
#endif

#if 1 // Memory test

#if 0
    while (!button_pressed()){}
    print("Ram test:\n");
    for (int i=0; i<64; i++) {
   *(volatile uint32_t*)(0x40000000 + i*4) = i;
    }
    print("Write Done, Reading\n");
    for (int i=0; i<64; i++) {
   print_int(*(volatile uint32_t*)(0x40000000 + i*4), 1);
   print(" ");
    }
    print("\n");

    print("Ram 2 test:\n");
    for (int i=0; i<64; i++) {
   *(volatile uint32_t*)(0x44000000 + i*4) = i;
    }
    print("Write Done, Reading\n");
    for (int i=0; i<64; i++) {
   print_int(*(volatile uint32_t*)(0x44000000 + i*4), 1);
   print(" ");
    }
    print("\n");
#else
    DDR_memtest();
#endif
#endif


#if 0
    // This test simply loops through test patterns.
    int pattern_nr = 0;
    int const_color_nr = 0;

    while(1){
        wait(3000000);
        pattern_nr = (pattern_nr + 1) % 7;
        REG_WR(TEST_PATTERN_NR, pattern_nr);

        if (pattern_nr == 0){
            const_color_nr = (const_color_nr + 1)%5;

            switch(const_color_nr){
                case 0: REG_WR(TEST_PATTERN_CONST_COLOR, 0x000000); break;
                case 1: REG_WR(TEST_PATTERN_CONST_COLOR, 0xffffff); break;
                case 2: REG_WR(TEST_PATTERN_CONST_COLOR, 0x0000ff); break;
                case 3: REG_WR(TEST_PATTERN_CONST_COLOR, 0x00ff00); break;
                case 4: REG_WR(TEST_PATTERN_CONST_COLOR, 0xff0000); break;
            }
        }
    }
#endif

    while(1){
        if (!button_pressed()){
            REG_WR(LED_WRITE, 0xff);
            wait(WAIT_CYCLES);
            REG_WR(LED_WRITE, 0x0);
            wait(WAIT_CYCLES);
        }
        else{
            REG_WR(LED_WRITE, 0x00);
        }
    }
}

uint32_t DDR_generate_test_word(uint32_t input) {
   return(input << 24) | (input << 12) | input;
}

#define DDR (volatile uint32_t *) 0x40000000
#define ONE_MEGABYTE             0x100000
#define DDR_TOTAL                (128 * ONE_MEGABYTE)

void DDR_memtest()
{
   volatile uint32_t *ptr;
   volatile uint8_t *base_byte = (uint8_t *)DDR;
   int i;

   int counter;
   ptr = DDR;
   counter = 0;
   print("Testing DDR\n");
   print("Filling memory wtih test pattern:\n");
   for(int i = 0; i < (DDR_TOTAL/ONE_MEGABYTE); i++) {
      //ptr = DDR + i * (1024/4); 
      for(int j = 0; j < (ONE_MEGABYTE/4); j++) {
         *ptr++ = DDR_generate_test_word(counter);
         counter ++;
      }
      print_int(i + 1,1);
      print("\r");
   }
   ptr = DDR;
   counter = 0;
   print("\nChecking test pattern:\n");
   for(i = 0; i < (DDR_TOTAL/ONE_MEGABYTE); i++) { // (DDR_TOTAL/1024)
      for(int j = 0; j < (ONE_MEGABYTE/4); j++) { // (1024/4)
         uint32_t dat = *ptr;
         if(dat != DDR_generate_test_word(counter)) {
            print("\nFailed at word ");
            print_int((uint32_t)ptr, 1);
            print(": ");
            print_int((uint32_t)(dat), 1);
            print(" Expected: ");
            print_int((uint32_t)(DDR_generate_test_word(counter)), 1);
            print("\n");
            for( ; ; );
         }
         ptr++;
         counter++;
      }
      print_int(i + 1,1);
      print("\r");
   }
   print("\nMemory test passed.\n");
}

