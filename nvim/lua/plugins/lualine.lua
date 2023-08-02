require('lualine').setup {
    sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = {
        { 'filename', file_status = true, path = 1 }
        },
        lualine_x = { 'filesize', 'encoding', 'fileformat' },
        lualine_y = { 'location' },
        lualine_z = { { 'filetype', colored = false, icon_only = false, icon = { align = 'left' } } },
    }
}
