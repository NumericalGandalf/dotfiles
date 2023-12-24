LsFt = {
    "lua",
    "python",
    "sh",
    "zsh",
}

local ls_def = {}
vim.g.IsDef = function(ft)
    if not ls_def[ft] then
        ls_def[ft] = true
        print(string.format("Defining LS settings for %s", ft))
        return false
    end
    return true
end
