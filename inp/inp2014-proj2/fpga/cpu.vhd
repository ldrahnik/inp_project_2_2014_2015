-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2014 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): DOPLNIT
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru

   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti

   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data

   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

-- Program counter PC signals
signal pc_reg        : std_logic_vector(12 downto 0);
signal pc_ld         : std_logic;                 -- signal, ktery v '1' rika, ze to chci ulozit
signal pc_inc        : std_logic;

-- Instruction register IREG
signal ireg_reg      : std_logic_vector(7 downto 0);
signal ireg_ld       : std_logic;                 -- signal, ktery v '1' rika, ze to chci ulozit

-- Data pointer
signal data_reg      : std_logic_vector(12 downto 0);
signal data_inc      : std_logic;
signal data_dec      : std_logic;
signal data_ld       : std_logic;

-- Instruction decoder
type inst_type is (dec_ptr, inc_ptr, inc_value, dev_value, while_start, while_end, print_value, read_value, halt);
signal ireg_decode   : inst_type;

-- Data or Program
signal acces_to_data : boolean := true;

begin


-- Program counter PC
pc_cntr: process (RESET, CLK)
begin
   if (RESET='1') then
      pc_reg <= (others=>'0');
   elseif (CLK'event) and (CLK='1') then
      if (pc_dec = '1') and (pc_inc = '0') then
         pc_reg <= pc_reg - 1;
      end if;
      if (pc_dec = '0') and (pc_inc = '1') then
            pc_reg <= pc_reg + 1;
      end if;
   end if;
end process


-- Instruction register IREG
ireg: process (RESET, CLK)
begin
   if (RESET='1') then
      ireg_reg <= (others=>'0');
   elseif (CLK'event) and (CLK='1') then
      if (ireg_ld='1') then
         ireg_reg <= DATA_RDATA;
      end if;
   end if;
end process


-- Data pointer
data_ptr: process(CLK, RESET)
   begin
      if (RESET='1') then
         data_reg <= "1000000000000";
      elsif (RESET = '0') and (CLK'event) and (CLK = '1') then
      if (PTR_decrement = '1') and (PTR_increment = '0') then           -- decrement
         data_reg <= data_reg - 1;
      end if;

      if (PTR_decrement = '0') and (PTR_increment = '1') then           -- increment
         data_reg <= data_reg + 1;
      end if;
   end if;
end process;



-- Switch between two memories (Von Neumann)
access: process()
   begin
   if (not(access_to_data)) then
      DATA_ADDR <= pc_reg;
   else
      DATA_ADDR <= data_reg;
   end if;
end process;


-- Instruction decoder -> decode ASCII value to instruction
process (ireg)
begin
   case ireg is
      when X"3C" => ireg_decode <= dec_ptr;              --    "<"
      when X"3E" => ireg_decode <= inc_ptr;              --    ">"
      when X"2B" => ireg_decode <= inc_value;            --    "+"
      when X"2D" => ireg_decode <= dec_value;            --    "-"
      when X"5B" => ireg_decode <= while_start;          --    "["
      when X"5D" => ireg_decode <= while_end;            --    "]"
      when X"2E" => ireg_decode <= print_value;          --    "."
      when X"2C" => ireg_decode <= read_value;           --    ","
      when X"00" => ireg_decode <= null;                 --    "null"
      when others => ireg_decode <= skip;                --    skip others
   end case;
end process;


-- Final state machine
fsm: process(present_state, ireg_decode, OUT_DATA)
begin
      next_state <= s00;

      DATA_EN  <= '0';
      dec_ptr  <= '0';
      inc_ptr  <= '0';
      ireg_ld  <= '0';
      pc_reg   <= '0';
      pc_ld    <= '0';
      pc_inc   <= '0';
      data_ld  <= '0';

      case present_state is

         when s_00 =>
            next_state <= s_0;

          -- Load instruction
         when s_0 =>
            next_state <= s_1;
            pc_ld <= '1';
            DATA_EN <= '1';

         -- Load instruction to instruction register
         when s_1 =>
            next_state <= s_2;
            ireg_ld <= '1';

          -- Decode instruction
         when s_2 =>
            case ireg_decode is
               when null =>
                  next_state <= s_null;

               when inc_ptr =>
                  next_state <= s_inc_ptr;

               when dec_ptr =>
                  next_state <= s_dec_ptr;

               when inc_val =>
                  next_state <= s_inc_value;

               when dec_val =>
                  next_state <= s_dec_value;

               when while_start =>
                  next_state <= s_while_start;

               when while_end =>
                  next_state <= s_while_end;

               when print_value =>
                  next_state <= s_print_value;

               when read_value =>
                  next_state <= s_read_value;

               when skip =>
                  next_state <= s_skip;
            end case;


         -- null
         when s_null =>
            next_state <= s_null;

         -- skip
         when s_skip =>
            pc_dec <= '0';
            pc_inc <= '1';
            next_state <= s_0;

         -- ">"      ->  PTR <- PTR + 1
         --          ->  PC  <- PC  + 1
         when s_inc_ptr =>
            data_dec <= '0';
            data_inc <= '1';
            pc_dec <= '0';
            pc_inc <= '1';
            next_state <= s_0;

         -- "<"      ->  PTR <- PTR - 1
         --          ->  PC  <- PC  + 1
         when s_dec_ptr =>
            data_dec <= '1';
            data_inc <= '0';
            pc_dec <= '0';
            pc_inc <= '1';
            next_state <= s_0;

         -- "+"      ->  DATA_RDATA <- ram[PTR]
         --          ->  ram[PTR] <- DATA_RDATA + 1
         --          ->  PC <- PC + 1
         when s_inc_value =>
            DATA_EN <= '1';
            data_ld <= '1';
            DATA_RDWR <= '0';
            next_state <= s_inc_value_2;

         -- "+"
         when s_inc_value_2 =>
            data_ld <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            DATA_WDATA <= DATA_RDATA + 1;

            pc_inc <= '0';
            pc_dec <= '1';
            next_state <= s_0;

         -- "-"      ->  DATA_RDATA <- ram[PTR]
         --          ->  ram[PTR] <- DATA_RDATA - 1; PC <- PC + 1
         when s_dec_val =>
            DATA_EN <= '1';
            data_ld <= '1';
            DATA_RDWR <= '0';
            next_state <= s_dec_value_2;

         when s_dec_val_2 =>
            data_ld <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            DATA_WDATA <= DATA_RDATA - 1;

            PC_decrement <= '0';
            PC_increment <= '1';
            next_state <= s_0;

         -- "."      ->  OUT_DATA <- ram[PTR]
         --          ->  PC       <- PC + 1
         when s_print_value =>
            DATA_EN     <= '1';
            data_ld     <= '1';
            DATA_RDWR   <= '0';
            next_state <= s_print_value_2;

         -- "."
         when s_print_value_2 =>
            if (OUT_BUSY = '0') then
               OUT_WE <= '1';
               OUT_DATA <= DATA_RDATA;
               PC_decrement <= '0';
               PC_increment <= '1';
               next_state <= s_0;
            else
               next_state <= s_print_value_2;
            end if;

         -- others
         when others =>
            next_state <= s_0;

      end case;
   end process;

end behavioral;

