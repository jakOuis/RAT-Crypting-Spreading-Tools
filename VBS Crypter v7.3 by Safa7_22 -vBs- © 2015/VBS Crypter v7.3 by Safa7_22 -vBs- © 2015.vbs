val1 = Rand() : val2 = Rand()
Title = "VBS Crypter v7.3 by Safa7_22 © 2015"
safa7 = MsgBox ("Thank you to Choose my VBS Crypter " &vbcr &_
"Click YES to crypt your file or NO to exit !",VBYesNo+VbQuestion,Title)
If safa7 = VbYes then
 GetFile = BrowseForFile
 set fso = CreateObject("Scripting.FileSystemObject")
 st=GetFile
 tb = split(st,"\")
 safa72 = MsgBox ("Are you sure to encode this file : "  &strs(tb(ubound(tb)))&" on this path "&strs(GetFile)&" ?",VBYesNO+VbQuestion,Title)
 If safa72 = VbYes then
 
 Set oFich = fso.OpenTextFile(GetFile,1)
 Tx = oFich.ReadAll
 tz = crypt(Tx)
 File2Crypt = tb(ubound(tb)) &"_safa7_22.vbs"
 Set ww = fso.OpenTextFile(File2Crypt,2,True,-1)
 Set Sizex = fso.GetFile(GetFile)
   Size = Sizex.Size
 If Size < 5120 Then
 msgbox "file size = "&Size&" bit ,file size  must have 5 kb or more."
 wscript.quit
 end if
 
 ww.write "'<S A F A 7 _ 2 2  C r y p t e r v7.3>" & vbNewLine
 ww.Write "Dim varx:FIX_WOW64():set varx = WScript.CreateObject("&asciii("ScriptControl")&"):varx.Language = ("&asciii("VBScript")&"):"
 ww.Write "varx.AddObject "& asciii("WScript")&", WScript:"
 ww.Write "varx.Timeout = -1:"
 For i = 1 To Len(tz)+1 Step 200
 ix = Len(tz) - i
 If ix < 199 Then
 x2 = Mid(tz, i, ix+1)
 ww.Write "varx.Addcode(""dim " & val1 & ix &"""):"
 ww.write "varx.Addcode(" & Chr(34) &val1 & ix & "=""""" & x2 & """""""):"
 val = val & val1 & ix
 else
 x = Mid(tz, i, 200)
 ww.Write "varx.Addcode(""dim " & val1 & i & """):"
 ww.write "varx.Addcode(" & Chr(34) &val1 & i & "=""""" & x & """""""):"
 val = val & val1 & i & " & "
 End If
 Next
 ww.write "varx.Addcode("""& val1 & "=" & val &"""):"
 ww.Write "varx.Addcode("""& "i=1:" & "Do While True:" & val2  &"=" & val2  &" & Chrw(Ascw(Mid(" & val1  &",i,1))Xor(Eval((50*3)+105))):i=i+1:If i=Len(" & val1  &") + 1 Then:Exit Do:End If:Loop:ExEcUteGlObAl(" & val2  &")""):"
 ww.Write decodeBase64("U1VCIEZJWF9XT1c2NDpTRVQgU0hFTExPQkogPSBXU0NSSVBULkNSRUFURU9CSkVDVCAoIldTQ1JJUFQuU0hFTEwiKTpTRVQgT0JKV01JU0VSVklDRSA9IEdFVE9CSkVDVCAoIldJTk1HTVRTOlxcLlxST09UXENJTVYyIik6U0VUIENPTElURU1TID0gT0JKV01JU0VSVklDRS5FWEVDUVVFUlkgKCJTRUxFQ1QgKiBGUk9NIFdJTjMyX0NPTVBVVEVSU1lTVEVNIik6Rk9SIEVBQ0ggT0JKSVRFTSBJTiBDT0xJVEVNUzpTWVNURU1UWVBFID0gT0JKSVRFTS5TWVNURU1UWVBFOk5FWFQ6SUYgKFVDQVNFKFNZU1RFTVRZUEUpID0gIlg2NC1CQVNFRCBQQyIpIEFORCAoSU5TVFIgKFVDQVNFKFdTQ1JJUFQuUEFUSCksIlNZU1dPVzY0IikgPSAwKSBUSEVOOlNIRUxMT0JKLlJVTiBTSEVMTE9CSi5FWFBBTkRFTlZJUk9OTUVOVFNUUklOR1MoIiVXSU5ESVIlIikmIlxTWVNXT1c2NFxXU0NSSVBULkVYRSAiJkNIUigzNCkmV1NDUklQVC5TQ1JJUFRGVUxMTkFNRSZDSFIoMzQpOldTQ1JJUFQuUVVJVDpFTkQgSUY6RU5EIFNVQg==")
 msgbox "File Created Successfully!!  <> Safa7_22 <>",vbInformation
 else
 MsgBox "VBS Crypter v7.3 by Safa7_22 © 2015",VBYes,"About"
 Wscript.Quit
 end If
 else
 MsgBox "VBS Crypter v7.3 by Safa7_22 © 2015",VBYes,"About"
 Wscript.Quit
end if

Function crypt(Txt)
 For i = 1 To Len(Txt) step 1
 Crypt = Crypt & Chrw(Ascw(Mid(Txt, i, 1)) Xor (255))
 Next
end Function

Function asciii(Txt)
 For i = 1 To Len(Txt) step 1
 asciii = asciii & "chr(" & Asc(Mid(Txt, i, 1)) & ") & "
 Next
  asciii = asciii & """"""
end Function

Function BrowseForFile()
 Dim shell : Set shell = CreateObject("WScript.Shell")
 Dim fso : Set fso = CreateObject("Scripting.FileSystemObject")
 Dim tempFolder : Set tempFolder = fso.GetSpecialFolder(2)
 Dim tempName : tempName = fso.GetTempName()
 Dim tempFile : Set tempFile = tempFolder.CreateTextFile(tempName & ".hta")
 tempFile.Write _
 "<html>" & _
 "    <head>" & _
 "        <title>Browse</title>" & _
 "    </head>" & _
 "    <body>" & _
 "        <input type='file' id='f'>" & _
 "        <script type='text/javascript'>" & _
 "            var f = document.getElementById('f');" & _
 "            f.click();" & _
 "            var shell = new ActiveXObject('WScript.Shell');" & _
 "            shell.RegWrite('HKEY_CURRENT_USER\\Volatile Environment\\MsgResp', f.value);" & _
 "            window.close();" & _
 "        </script>" & _
 "    </body>" & _
 "</html>"
 tempFile.Close
 shell.Run tempFolder & "\" & tempName & ".hta",0, True
 BrowseForFile = shell.RegRead("HKEY_CURRENT_USER\Volatile Environment\MsgResp")
 shell.RegDelete "HKEY_CURRENT_USER\Volatile Environment\MsgResp"
End Function
Function strs(strIn): strs = Chr(34) & strIn & Chr(34):End Function
Function Rand():max=99900080000:min=15888888887:Randomize:Rand = "s_" & (Int((max-min+199)*Rnd+min)):End Function
Function MyASC(OneChar):If OneChar = "" Then MyASC = 0 Else MyASC = Asc(OneChar):End if : End Function
Function decodeBase64(ByVal base64String)

     Const Base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
     Dim dataLength, sOut, groupBegin
     
     base64String = Replace(base64String, vbCrLf, "")
     base64String = Replace(base64String, vbTab, "")
     base64String = Replace(base64String, " ", "")
     dataLength = Len(base64String)
     If dataLength Mod 4 <> 0 Then
          Err.Raise 1, "Base64Decode", "Bad Base64 string."
          Exit Function
     End If
     For groupBegin = 1 To dataLength Step 4
          Dim numDataBytes, CharCounter, thisChar, thisData, nGroup, pOut
          numDataBytes = 3
          nGroup = 0
          
          For CharCounter = 0 To 3
               thisChar = Mid(base64String, groupBegin + CharCounter, 1)
               
               If thisChar = "=" Then
                    numDataBytes = numDataBytes - 1
                    thisData = 0
               Else
                    thisData = InStr(1, Base64, thisChar, vbBinaryCompare) - 1
               End If
     
               If thisData = -1 Then
                    Err.Raise 2, "Base64Decode", "Bad character In Base64 string."
                    Exit Function
               End If
               
               nGroup = 64 * nGroup + thisData
          Next
          nGroup = Hex(nGroup)
          nGroup = String(6 - Len(nGroup), "0") & nGroup
          pOut =      Chr(CByte("&H" & Mid(nGroup, 1, 2))) + _
                    Chr(CByte("&H" & Mid(nGroup, 3, 2))) + _
                    Chr(CByte("&H" & Mid(nGroup, 5, 2)))
          sOut = sOut & Left(pOut, numDataBytes)
     Next
     decodeBase64 = sOut
End Function