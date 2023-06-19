library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package Main_topEntity_types is

  subtype Maybe is std_logic_vector(8 downto 0);
  type array_of_Maybe is array (integer range <>) of Maybe;
  subtype RowOperation is std_logic_vector(145 downto 0);
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (value :  array_of_Maybe) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Maybe;
end;

package body Main_topEntity_types is
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return signed(islv);
  end;
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function toSLV (value :  array_of_Maybe) return std_logic_vector is
    alias ivalue    : array_of_Maybe(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 9);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 9) + 1 to i*9) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Maybe is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Maybe(0 to slv'length / 9 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 9 to (i+1) * 9 - 1));
    end loop;
    return result;
  end;
end;

