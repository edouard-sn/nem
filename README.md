# nem
NES Emulator project

This branch, `cpu/demo/snake`, is a very rudimantary branch that is used to showcase the CPU with a snake game.
The graphical part has been hackily patched onto the CPU, this ""architechture"" will not be used in the future.


<table><thead><tr><th>Address space</th><th>Type</th><th>Description</th></tr></thead><tbody>
<tr><td><strong>0xFE</strong></td><td>Input</td><td>Random Number Generator</td></tr>
<tr><td><strong>0xFF</strong></td><td>Input</td><td>A code of the last pressed Button</td></tr>
<tr><td><strong>[0x0200..0x0600]</strong></td><td>Output</td><td>Screen.<br>Each cell represents the color of a pixel in a 32x32 matrix.<br><br> The matrix starts from top left corner, i.e.<br><br> <strong>0x0200</strong> - the color of (0,0) pixel <br> <strong>0x0201</strong> - (1,0) <br> <strong>0x0220</strong> - (0,1) <br><br> <div style="text-align:left"><img src="https://bugzmanov.github.io/nes_ebook/images/ch3.4/image_2_screen_matrix.png" width="50%"></div></td></tr>
<tr><td><strong>[0x0600...]</strong></td><td>Game code</td><td>Execution code</td></tr>
</tbody></table>

> Illustration took from first [reference](#references)


# References

[First game with 6502 CPU](https://bugzmanov.github.io/nes_ebook/chapter_3_4.html)