BEGIN {
  FS="/"
}

{
  if (NF < 3) i=1; else i=NF-2;
  for (;i<=NF;i++) {
    print $i "|"; 
  }
}
