local M = {}

function M.arr_contains(table_, val)
	for i = 1, #table_ do
		if table_[i] == val then
			return true
		end
	end
	return false
end

return M
