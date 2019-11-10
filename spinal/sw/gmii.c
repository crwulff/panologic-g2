#include <stdint.h>
#include <math.h>

#include "reg.h"
#include "top_defines.h"
#include "print.h"

// Set to 1 to dump packets
#define DUMP_RX_PACKETS 0
#define DUMP_TX_PACKETS 0

#define FILE_BUFFER 0x44000000

static uint8_t myMAC[6] = { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05 };
static const uint8_t broadcastMAC[6] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };

static inline uint32_t rdcycle(void) {
    uint32_t cycle;
    asm volatile ("rdcycle %0" : "=r"(cycle));
    return cycle;
}

static inline int nop(void) {
    asm volatile ("addi x0, x0, 0");
    return 0;
}

static void wait(int cycles)
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

typedef enum {
    eBS_BOOTP_REQ,
    eBS_ARP,
    eBS_TFTP_RRQ,
    eBS_TFTP_XFER,
    eBS_XFER_DONE
} EBootState;

static EBootState bootState = eBS_BOOTP_REQ;

void gmii_mdio_init()
{
    // Initial values
    REG_WR(GMII_MDIO,     (1<<GMII_MDC_VAL_BIT)
                        | (0<<GMII_MDIO_ENA_BIT)            // MDIO starts out tri-state
                        | (1<<GMII_MDIO_WR_BIT)
        );
}

#define MII_HALF_BIT_WAIT  1

#define GMII_MDC_SET()      (REG_WR(GMII_MDIO, REG_RD(GMII_MDIO) |  (1<<GMII_MDC_VAL_BIT)))
#define GMII_MDC_CLR()      (REG_WR(GMII_MDIO, REG_RD(GMII_MDIO) & ~(1<<GMII_MDC_VAL_BIT)))

#define GMII_MDIO_ENA()     (REG_WR(GMII_MDIO, REG_RD(GMII_MDIO) |  (1<<GMII_MDIO_ENA_BIT)))
#define GMII_MDIO_DIS()     (REG_WR(GMII_MDIO, REG_RD(GMII_MDIO) & ~(1<<GMII_MDIO_ENA_BIT)))

#define GMII_MDIO_SET()     (REG_WR(GMII_MDIO, REG_RD(GMII_MDIO) |  (1<<GMII_MDIO_WR_BIT)))
#define GMII_MDIO_CLR()     (REG_WR(GMII_MDIO, REG_RD(GMII_MDIO) & ~(1<<GMII_MDIO_WR_BIT)))

#define GMII_MDIO_RD()      ((REG_RD(GMII_MDIO) >> GMII_MDIO_RD_BIT) & 1)

void gmii_mdc_toggle()
{
    wait(MII_HALF_BIT_WAIT);
    GMII_MDC_SET();
    wait(MII_HALF_BIT_WAIT);
    GMII_MDC_CLR();
}

int gmii_mdio_rd(int phy_addr, int reg_addr)
{
    for(int i=0;i<32;++i){
        gmii_mdc_toggle();
    }

    GMII_MDIO_ENA();

    unsigned word =   (1 << 12)                     // Start bits
                    | (2 << 10)                     // Read
                    | ((phy_addr & 0x1f) << 5)
                    | ((reg_addr & 0x1f) << 0);

    for(int i=13; i >= 0; --i){
        int bit = (word >> i) & 1;

        if (bit) GMII_MDIO_SET();
        else     GMII_MDIO_CLR();

        gmii_mdc_toggle();
    }

    int ta = 0;
    int rdata = 0;

    GMII_MDIO_DIS();
    gmii_mdc_toggle();

    ta = GMII_MDIO_RD();
    gmii_mdc_toggle();
    ta = (ta<<1) | GMII_MDIO_RD();

    for(int i=15;i>=0;--i){
        rdata = (rdata<<1) | GMII_MDIO_RD();
        gmii_mdc_toggle();
    }

    return rdata;
}


void gmii_phy_identifier(int phy_addr, uint32_t *oui, uint32_t *model_nr, uint32_t *rev_nr)
{
    int rdata2 = gmii_mdio_rd(phy_addr, 2);
    int rdata3 = gmii_mdio_rd(phy_addr, 3);

    *oui      = (((rdata3 >> 10) & ((1<<6)-1))<< 0) | (rdata2 << 6);

    *model_nr = (rdata3 >> 4) & ((1<<6)-1);
    *rev_nr   = (rdata3 >> 0) & ((1<<4)-1);
}

#if 1
void gmii_reg_dump(int phy_addr)
{
    int rdata;

    rdata = gmii_mdio_rd(phy_addr, 0);
    print("Reg  0: Control               : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 1);
    print("Reg  1: Status                : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 2);
    print("Reg  2: PHY ID                : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 3);
    print("Reg  3: PHY ID                : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 4);
    print("Reg  4: Auto-Neg Advertisement: "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 5);
    print("Reg  5: Link Partner Ability  : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 6);
    print("Reg  6: Auto-Neg Expansion    : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 16);
    print("Reg 16: PHY Specific Control  : "); print_int(rdata, 1); print("\n");

    rdata = gmii_mdio_rd(phy_addr, 17);
    print("Reg 17: PHY Specific Status   : "); print_int(rdata, 1); print("\n");
}
#endif

void gmii_wait_auto_neg_complete(int phy_addr)
{
    int rdata;

    do{
        rdata = gmii_mdio_rd(phy_addr, 1);
    } while(!(rdata & (1<<5)));
}

#if 1
void gmii_print_phy_id(int phy_addr)
{
    uint32_t oui, model_nr, rev_nr;

    gmii_phy_identifier(phy_addr, &oui, &model_nr, &rev_nr);
    print("oui      :");
    print_int(oui, 1);
    print("\n");
    print("model_nr :");
    print_int(model_nr, 1);
    print("\n");
    print("rev_nr   :");
    print_int(rev_nr, 1);
    print("\n");
}

void gmii_monitor_regs(int phy_addr)
{
    int prev_rdata = gmii_mdio_rd(phy_addr, 17);
    while(1){
        int rdata = gmii_mdio_rd(phy_addr, 17);

        if (rdata != prev_rdata){
            gmii_reg_dump(phy_addr);
            prev_rdata = rdata;
        }
    }
}
#endif

const uint32_t crc32_table[256] = {
    0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419,
    0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4,
    0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07,
    0x90bf1d91, 0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
    0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856,
    0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
    0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4,
    0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
    0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3,
    0x45df5c75, 0xdcd60dcf, 0xabd13d59, 0x26d930ac, 0x51de003a,
    0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599,
    0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
    0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190,
    0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f,
    0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e,
    0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
    0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed,
    0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
    0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3,
    0xfbd44c65, 0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
    0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a,
    0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5,
    0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa, 0xbe0b1010,
    0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
    0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17,
    0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6,
    0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615,
    0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
    0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1, 0xf00f9344,
    0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
    0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a,
    0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
    0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1,
    0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c,
    0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef,
    0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
    0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe,
    0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31,
    0x2cd99e8b, 0x5bdeae1d, 0x9b64c2b0, 0xec63f226, 0x756aa39c,
    0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
    0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b,
    0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
    0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1,
    0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
    0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45, 0xa00ae278,
    0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7,
    0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66,
    0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
    0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605,
    0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8,
    0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b,
    0x2d02ef8d
};

uint32_t crc32(uint8_t *buf, unsigned int len)
{
    unsigned int i;
    uint32_t crc32 = 0xFFFFFFFF;

    for (i = 0; i < len; i++) {
        crc32 = crc32_table[(crc32 ^ buf[i]) & 0xff] ^ (crc32 >> 8);
    }

    return ( ~crc32 );
}

void gmii_tx_packet(uint8_t *buf, int len)
{
    // Pad out the packet
    for (; len<60; len++) {
        buf[len] = 0x00;
    }

    // Calculate FCS
    uint32_t crc = crc32(buf, len);

    //
    // Output Packet
    //

    // Preamble
    for (int i=0; i<7; i++) {
        REG_WR(GMII_TX_FIFO_WR, 0x55);
    }

    // SFD
    REG_WR(GMII_TX_FIFO_WR, 0xd5);

    // Payload
    for (int i=0; i<len; i++) {
        REG_WR(GMII_TX_FIFO_WR, buf[i]);
    }

    // FCS + EOP Flag
    for (int i=0; i<4; i++) {
        REG_WR(GMII_TX_FIFO_WR, (crc & 0xFF) | ((i == 3) ? 0x100 : 0));
        crc >>= 8;
    }
}

static uint32_t myIP = 0;
static uint32_t serverIP = 0;
static char     bootFile[129] = {0};
static uint8_t  serverMAC[6] = {0};
static uint16_t tftpBlock = 0;
static uint16_t tftpSrcPort = 0;

void send_arp_request(uint32_t targetIP);
void send_tftp_request(void);
void send_tftp_ack(void);

void gmii_rx_ip_packet(uint8_t *buf, unsigned int len) {
    uint8_t proto = buf[23];
    uint16_t srcPort = (buf[34] << 8) | buf[35];
    uint16_t destPort = (buf[36] << 8) | buf[37];

    switch (bootState) {
        case eBS_BOOTP_REQ:
            // Looking for a BOOTP response
            if (proto == 17) { // UDP
                if (srcPort == 67 && destPort == 68 && buf[42] == 0x02) { // BOOTP response
                    myIP = (buf[58] << 24) | (buf[59] << 16) | (buf[60] << 8) | buf[61];
                    serverIP = (buf[62] << 24) | (buf[63] << 16) | (buf[64] << 8) | buf[65];
                    for (int i=0; i<128; i++) {
                        bootFile[i] = buf[i+0x96];
                    }
                    bootFile[128] = 0;

                    bootState = eBS_ARP;
                    print("BOOTP RESP - IP: ");
                    print_int(myIP, 1);
                    print(", Server IP: ");
                    print_int(serverIP, 1);
                    print(", Boot File: ");
                    print(bootFile);
                    print("\n");

                    send_arp_request(serverIP);
                }
            }
            break;

        case eBS_TFTP_RRQ:
        case eBS_TFTP_XFER:
            // Looking for TFTP DATA
            if (destPort == 12345) {
                uint16_t opcode = (buf[42] << 8) | buf[43];
                if (opcode == 0x0003) {
                    uint16_t block = (buf[44] << 8) | buf[45];
                    if (block == tftpBlock + 1) {
                        tftpBlock = block;
                        uint8_t *outputBuffer = (uint8_t*) (FILE_BUFFER + 512 * (block-1));
                        for (unsigned int i=0; i<len-46; i++) {
                            outputBuffer[i] = buf[i+46];
                        }

                        bootState = (len-46 < 512) ? eBS_XFER_DONE : eBS_TFTP_XFER;
                    }
                    tftpSrcPort = srcPort;
                    print("#");
                    send_tftp_ack();

                } else if (opcode == 0x0005) {
                    bootState = eBS_TFTP_RRQ;
                }
            }
            break;

        default:
            break;
    }
}

void gmii_rx_arp_packet(uint8_t *buf, unsigned int len) {
    uint16_t opcode = (buf[0x14] << 8) | buf[0x15];
    uint32_t senderIP = (buf[0x1c] << 24) | (buf[0x1d] << 16) | (buf[0x1e] << 8) | buf[0x1f];

    switch (bootState) {
        case eBS_ARP:
            // Looking for an ARP response
            if (opcode == 0x0002 && senderIP == serverIP) {
                for (int i=0; i<6; i++) {
                    serverMAC[i] = buf[0x16 + i];
                }
                print("ARP Response\n");
                bootState = eBS_TFTP_RRQ;
                send_tftp_request();
            }
            break;

        default:
            break;
    }
}

void gmii_rx_packet(uint8_t *buf, unsigned int len)
{
    for (int i=0; i<6; i++) {
        if (buf[i] != myMAC[i]) {
            return;
        }
    }

    // Packet is for me

#if 0
    for (unsigned int i=0; i<len; i++) {
      print_byte(buf[i], 1);
      print(",");
    }
    print("/n/n");
#endif

    uint16_t ethertype = (buf[12] << 8) | buf[13];

    switch (ethertype) {
        case 0x0800: // IP
            gmii_rx_ip_packet(buf, len);
            break;

        case 0x0806:
            gmii_rx_arp_packet(buf, len);
            break;

        default:
            break;
    }
}

int add_unicast_mac_header(uint8_t *buf, const uint8_t *destMAC, uint16_t ethertype) {

    int idx = 0;

    // Dest MAC
    for (int i=0; i<6; i++) {
        buf[idx++] = destMAC[i];
    }

    // Src MAC
    for (int i=0; i<6; i++) {
        buf[idx++] = myMAC[i];
    }

    // Ethertype
    buf[idx++] = ethertype >> 8;
    buf[idx++] = ethertype & 0xFF;

    return idx;
}

int add_broadcast_mac_header(uint8_t *buf, uint16_t ethertype) {
    return add_unicast_mac_header(buf, broadcastMAC, ethertype);
}

int add_ip_header(uint8_t *buf, uint32_t srcIP, uint32_t destIP, uint16_t len, uint8_t proto) {
    int idx = 0;

    len += 20; // IP header length

    buf[idx++] = 0x45;
    buf[idx++] = 0x00;
    buf[idx++] = len >> 8;
    buf[idx++] = len & 0xFF;
    buf[idx++] = 0x00; // Identification
    buf[idx++] = 0x00; // Identification
    buf[idx++] = 0x40; // Don't fragment
    buf[idx++] = 0x00;
    buf[idx++] = 0x01; // TTL
    buf[idx++] = proto;
    buf[idx++] = 0x00; // csum
    buf[idx++] = 0x00; // csum
    buf[idx++] = srcIP >> 24;
    buf[idx++] = (srcIP >> 16) & 0xFF;
    buf[idx++] = (srcIP >> 8) & 0xFF;
    buf[idx++] = (srcIP & 0xFF);
    buf[idx++] = destIP >> 24;
    buf[idx++] = (destIP >> 16) & 0xFF;
    buf[idx++] = (destIP >> 8) & 0xFF;
    buf[idx++] = (destIP & 0xFF);

    uint32_t csum = 0;
    for (int i=0; i<idx; i+=2) {
        csum += (buf[i] << 8) | buf[i+1];
    }
    csum = (csum & 0xFFFF) + (csum >> 16);
    csum = (csum & 0xFFFF) + (csum >> 16);
    csum = csum ^ 0xFFFF;
    buf[10] = (csum >> 8) & 0xFF;
    buf[11] = csum & 0xFF;

    return idx;
}

int add_udp_header(uint8_t *buf, uint16_t srcPort, uint16_t destPort, uint16_t len) {
    int idx = 0;

    len += 8;

    buf[idx++] = srcPort >> 8;
    buf[idx++] = srcPort & 0xFF;
    buf[idx++] = destPort >> 8;
    buf[idx++] = destPort & 0xFF;
    buf[idx++] = len >> 8;
    buf[idx++] = len & 0xFF;
    buf[idx++] = 0x00; // csum
    buf[idx++] = 0x00; // csum

    return idx;
}

#define BOOTP_SERVER_PORT 67
#define BOOTP_CLIENT_PORT 68

#define PROTO_UDP 17

#define UDP_LEN 8

void send_bootp_request(void) {
    print("BOOTP REQ\n");
    uint8_t *buf = (uint8_t*)DDR_BASE_ADDR;
    int idx = 0;
    int len = 300;
    idx += add_broadcast_mac_header(&buf[idx], 0x0800);
    idx += add_ip_header(&buf[idx], 0x00000000, 0xFFFFFFFF, len + UDP_LEN, PROTO_UDP);
    idx += add_udp_header(&buf[idx], BOOTP_CLIENT_PORT, BOOTP_SERVER_PORT, len);

    buf[idx++] = 0x01; // BOOTP request
    buf[idx++] = 0x01; // Ethernet
    buf[idx++] = 0x06; // Ethernet HW address length
#if 1
    for (int i=0; i<25; i++) {
        buf[idx++] = 0x00;
    }
#else
    buf[idx++] = 0x00; // Hops
    buf[idx++] = 0x00; // transaction id
    buf[idx++] = 0x00; // transaction id
    buf[idx++] = 0x00; // transaction id
    buf[idx++] = 0x00; // transaction id
    buf[idx++] = 0x00; // seconds since boot
    buf[idx++] = 0x00; // seconds since boot
    buf[idx++] = 0x00; // unused
    buf[idx++] = 0x00; // unused
    buf[idx++] = 0x00; // client IP
    buf[idx++] = 0x00; // client IP
    buf[idx++] = 0x00; // client IP
    buf[idx++] = 0x00; // client IP
    buf[idx++] = 0x00; // your IP (filled by server in response)
    buf[idx++] = 0x00; // your IP (filled by server in response)
    buf[idx++] = 0x00; // your IP (filled by server in response)
    buf[idx++] = 0x00; // your IP (filled by server in response)
    buf[idx++] = 0x00; // server IP (filled by server in response)
    buf[idx++] = 0x00; // server IP (filled by server in response)
    buf[idx++] = 0x00; // server IP (filled by server in response)
    buf[idx++] = 0x00; // server IP (filled by server in response)
    buf[idx++] = 0x00; // gateway IP (filled by server in response)
    buf[idx++] = 0x00; // gateway IP (filled by server in response)
    buf[idx++] = 0x00; // gateway IP (filled by server in response)
    buf[idx++] = 0x00; // gateway IP (filled by server in response)
#endif

    // client hardware address
    for (int i=0; i<6; i++) {
        buf[idx++] = myMAC[i];
    }

    // server host name + filename (+10 spare hardware address bytes) + vend
    for (int i=0; i<10 + 64 + 128 + 64; i++) {
        buf[idx++] = 0x00;
    }

    gmii_tx_packet(buf, idx);
}

void send_arp_request(uint32_t targetIP) {
    print("ARP REQ - ");
    print_int(targetIP, 1);
    print("\n");

    uint8_t *buf = (uint8_t*)DDR_BASE_ADDR;
    int idx = 0;
    idx += add_broadcast_mac_header(&buf[idx], 0x0806);
    buf[idx++] = 0x00; // HW type
    buf[idx++] = 0x01; // HW type (ethernet)
    buf[idx++] = 0x08; // Protocol (IPv4)
    buf[idx++] = 0x00; // Protocol (IPv4)
    buf[idx++] = 0x06; // HW Address Size
    buf[idx++] = 0x04; // Protocol Address Size
    buf[idx++] = 0x00; // Opcode (request)
    buf[idx++] = 0x01; // Opcode (request)
    for (int i=0; i<6; i++) {
        buf[idx++] = myMAC[i];
    }
    buf[idx++] = (myIP >> 24) & 0xFF;
    buf[idx++] = (myIP >> 16) & 0xFF;
    buf[idx++] = (myIP >>  8) & 0xFF;
    buf[idx++] = myIP & 0xFF;
    for (int i=0; i<6; i++) {
        buf[idx++] = 0x00; // Target MAC
    }

    buf[idx++] = (targetIP >> 24) & 0xFF;
    buf[idx++] = (targetIP >> 16) & 0xFF;
    buf[idx++] = (targetIP >>  8) & 0xFF;
    buf[idx++] = targetIP & 0xFF;

    gmii_tx_packet(buf, idx);
}

int strlen(const char* s) {
    int len = 0;
    while (s[len++] != 0);
    return len;
}

void udp_checksum(uint8_t *buf, int len) {
    uint32_t csum = 0x11; // Protocol
    csum += (buf[0x26] << 8) | buf[0x27];

    for (int i=0x1a; i<len; i+=2) {
        csum += (buf[i] << 8) | buf[i+1];
    }
    csum = (csum & 0xFFFF) + (csum >> 16);
    csum = (csum & 0xFFFF) + (csum >> 16);
    csum = csum ^ 0xFFFF;
    buf[0x28] = (csum >> 8) & 0xFF;
    buf[0x29] = csum & 0xFF;
}

void send_tftp_request(void) {
    print("TFTP RRQ\n");

    uint8_t *buf = (uint8_t*)DDR_BASE_ADDR;
    int idx = 0;
    int len = 8 + strlen(bootFile);
    idx += add_unicast_mac_header(&buf[idx], serverMAC, 0x0800);
    idx += add_ip_header(&buf[idx], myIP, serverIP, len + UDP_LEN, PROTO_UDP);
    idx += add_udp_header(&buf[idx], 12345, 69, len);
    buf[idx++] = 0x00;
    buf[idx++] = 0x01; // RRQ
    for (int i=0; i<129; i++) {
        buf[idx++] = bootFile[i];
        if (bootFile[i] == 0) break;
    }
    buf[idx++] = 'o';
    buf[idx++] = 'c';
    buf[idx++] = 't';
    buf[idx++] = 'e';
    buf[idx++] = 't';
    buf[idx++] = 0;

    udp_checksum(buf, idx);

    gmii_tx_packet(buf, idx);
}

void send_tftp_ack(void) {

    //print("ACK - ");
    //print_int(tftpBlock, 1);
    //print("\n");

    uint8_t *buf = (uint8_t*)DDR_BASE_ADDR;
    int idx = 0;
    int len = 4;
    idx += add_unicast_mac_header(&buf[idx], serverMAC, 0x0800);
    idx += add_ip_header(&buf[idx], myIP, serverIP, len + UDP_LEN, PROTO_UDP);
    idx += add_udp_header(&buf[idx], 12345, tftpSrcPort, len);
    buf[idx++] = 0x00;
    buf[idx++] = 0x04; // ACK
    buf[idx++] = tftpBlock >> 8;
    buf[idx++] = tftpBlock & 0xFF;

    udp_checksum(buf, idx);

    gmii_tx_packet(buf, idx);
}

void gmii_tx_test_packet(void)
{
    uint8_t *buf = (uint8_t*)DDR_BASE_ADDR;
    int idx = 0;

    idx += add_broadcast_mac_header(&buf[idx], 0x88b5);

    for (int i=0; i<64; i++) {
        buf[idx++] = i | 0x80;
    }

    gmii_tx_packet(buf, idx);
}

void timerAction(void) {
    switch (bootState) {
        case eBS_BOOTP_REQ:
            send_bootp_request();
            break;

        case eBS_ARP:
            send_arp_request(serverIP);
            break;

        case eBS_TFTP_RRQ:
            send_tftp_request();
            break;

        case eBS_TFTP_XFER:
            print("T");
            send_tftp_ack();
            break;

        default:
            break;
    }
}

void gmii_dump_packets()
{
    int had_data = 0, sfd = 0;
    uint8_t *rxbuf = (uint8_t*)(DDR_BASE_ADDR + 2048);
    uint32_t loopCount = 0;

    while(1){
        if (loopCount % 1000000 == 0) {
           timerAction();
        }
        loopCount++;

        if (bootState == eBS_XFER_DONE) {
            print("\n\nBooting...\n");
            void (*bootFunc)(void) = (void (*)(void))FILE_BUFFER;
            bootFunc();
        }

        unsigned int rx_data = REG_RD(GMII_RX_FIFO_RD);
        int rx_has_data = (rx_data >> GMII_RX_FIFO_RD_VALID_BIT) & 1;
        int rx_data_valid = (rx_data >> GMII_RX_FIFO_RD_DV_BIT) & 1; // packet separator
        if (!rx_has_data || !rx_data_valid){
            if (had_data){
#if DUMP_RX_PACKETS
                print_int(had_data, 1);
#endif

                uint32_t packetcrc = rxbuf[had_data - 4] | (rxbuf[had_data - 3] << 8) | (rxbuf[had_data - 2] << 16) | (rxbuf[had_data - 1] << 24);
                if (rx_data & 0x100) {
#if DUMP_RX_PACKETS
                    print(" -- ERR");
#endif
                } else if (had_data > 4) {
                    uint32_t crc = crc32(rxbuf, had_data - 4);
#if DUMP_RX_PACKETS
                    print(",");
                    print_int(crc, 1);
#endif
                    if (crc != packetcrc) {
#if DUMP_RX_PACKETS
                        print(" -- BAD CRC\n\n");
#endif
                    } else {
                        gmii_rx_packet(rxbuf, had_data);
                    }
                }

#if DUMP_RX_PACKETS
                print("\n\n");
#endif
                //gmii_tx_test_packet();
            }
            had_data = 0;
            sfd = 0;
            continue;
        }

        if (!had_data){
#if DUMP_RX_PACKETS
            print(".");
#endif
        }

        if (!sfd) {
            if ((rx_data & 0xFF) == 0xd5) {
                sfd = 1;
            }
            continue;
        }

        rxbuf[had_data] = rx_data & 0xFF;

        had_data += 1;

#if DUMP_RX_PACKETS
        print_byte(rx_data, 1);
        print(",");
#endif
    }
}

