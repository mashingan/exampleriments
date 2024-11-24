with Interfaces.C; use Interfaces.C;

package C_API is
    function My_func (a : int) return int with
        Export => true,
        Convention => C,
        External_Name => "my_func";

end C_API;
