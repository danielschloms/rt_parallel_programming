package Typenames is
    subtype Int16 is Short_Integer;
    subtype Int32 is Long_Integer;
    subtype Int64 is Long_Long_Integer;
    -- Int128 too large
    subtype LLFloat is Long_Long_Float;
end Typenames;
