-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Main_topEntity_types.all;

entity topEntity is
  port(carg     : in Main_topEntity_types.RowOperation;
       result_0 : out Main_topEntity_types.Maybe;
       result_1 : out Main_topEntity_types.Maybe;
       result_2 : out Main_topEntity_types.Maybe;
       result_3 : out Main_topEntity_types.Maybe;
       result_4 : out Main_topEntity_types.Maybe;
       result_5 : out Main_topEntity_types.Maybe;
       result_6 : out Main_topEntity_types.Maybe;
       result_7 : out Main_topEntity_types.Maybe);
end;

architecture structural of topEntity is
  signal \c$mapOut\         : Main_topEntity_types.array_of_Maybe(0 to 7);
  signal \c$mapOut_app_arg\ : Main_topEntity_types.array_of_Maybe(0 to 7);
  signal \c$case_alt\       : Main_topEntity_types.array_of_Maybe(0 to 7);
  -- gauss_rows.hs:34:1-7
  signal row1               : Main_topEntity_types.array_of_Maybe(0 to 7);
  -- gauss_rows.hs:34:1-7
  signal row2               : Main_topEntity_types.array_of_Maybe(0 to 7);
  -- gauss_rows.hs:34:1-7
  signal row1_0             : Main_topEntity_types.array_of_Maybe(0 to 7);
  -- gauss_rows.hs:34:1-7
  signal row1_1             : Main_topEntity_types.array_of_Maybe(0 to 7);
  signal result             : Main_topEntity_types.array_of_Maybe(0 to 7);

begin
  -- map begin
  r_map : for i in \c$mapOut\'range generate
  begin
    fun_1 : block
      -- gauss_rows.hs:26:1-8
      signal b                       : signed(7 downto 0);
      signal result_16               : Main_topEntity_types.Maybe;
      signal \c$divMaybeOut_app_arg\ : signed(7 downto 0);
      -- gauss_rows.hs:34:1-7
      signal con                     : signed(7 downto 0);
      -- gauss_rows.hs:34:1-7
      signal con_0                   : signed(7 downto 0);
    begin
      \c$mapOut\(i) <= result_16;

      b <= signed'(main_topentity_types.fromSLV(\c$mapOut_app_arg\(i)(7 downto 0)));

      with (\c$mapOut_app_arg\(i)(8 downto 8)) select
        result_16 <= std_logic_vector'("0" & "--------") when "0",
                     std_logic_vector'("1" & (std_logic_vector(resize(\c$divMaybeOut_app_arg\ * b, 8)))) when others;

      with (carg(145 downto 144)) select
        \c$divMaybeOut_app_arg\ <= con when "01",
                                   con_0 when others;

      con <= signed'(main_topentity_types.fromSLV(carg(71 downto 64)));

      con_0 <= signed'(main_topentity_types.fromSLV(carg(71 downto 64)));


    end block;
  end generate;
  -- map end

  with (carg(145 downto 144)) select
    \c$mapOut_app_arg\ <= row1_0 when "01",
                          row1_1 when others;

  -- zipWith begin
  zipWith : for i_0 in \c$case_alt\'range generate
  begin
    fun_2 : block
      -- gauss_rows.hs:17:1-8
      signal ipv            : signed(7 downto 0);
      signal \c$case_alt_0\ : Main_topEntity_types.Maybe;
      -- gauss_rows.hs:17:1-8
      signal ipv1           : signed(7 downto 0);
      signal result_17      : Main_topEntity_types.Maybe;
    begin
      \c$case_alt\(i_0) <= result_17;

      ipv <= signed'(main_topentity_types.fromSLV(row1(i_0)(7 downto 0)));

      with (row2(i_0)(8 downto 8)) select
        \c$case_alt_0\ <= std_logic_vector'("0" & "--------") when "0",
                          std_logic_vector'("1" & (std_logic_vector(ipv + ipv1))) when others;

      ipv1 <= signed'(main_topentity_types.fromSLV(row2(i_0)(7 downto 0)));

      with (row1(i_0)(8 downto 8)) select
        result_17 <= std_logic_vector'("0" & "--------") when "0",
                     \c$case_alt_0\ when others;


    end block;
  end generate;
  -- zipWith end

  with (carg(145 downto 144)) select
    result <= \c$case_alt\ when "00",
              \c$mapOut\ when "01",
              \c$mapOut\ when others;

  row1 <= Main_topEntity_types.array_of_Maybe'(main_topentity_types.fromSLV(carg(143 downto 72)));

  row2 <= Main_topEntity_types.array_of_Maybe'(main_topentity_types.fromSLV(carg(71 downto 0)));

  row1_0 <= Main_topEntity_types.array_of_Maybe'(main_topentity_types.fromSLV(carg(143 downto 72)));

  row1_1 <= Main_topEntity_types.array_of_Maybe'(main_topentity_types.fromSLV(carg(143 downto 72)));

  result_0 <= result(0);

  result_1 <= result(1);

  result_2 <= result(2);

  result_3 <= result(3);

  result_4 <= result(4);

  result_5 <= result(5);

  result_6 <= result(6);

  result_7 <= result(7);


end;

