# Ada PDF Writer 

The Ada PDF Writer consists of a package, PDF_Out,
which produces PDF files - as "physical" files, or as
other types of data streams.

The creation of a PDF file is as simple as this
small procedure:

```Ada
  with PDF_Out;

  procedure Hello is
    pdf : PDF_Out.PDF_Out_File;
  begin
    pdf.Create ("tiny.pdf");
    pdf.Put_Line ("Hello world !");
    pdf.Close;
  end;
```

**Full description in: `pdf_writer.txt`**

* Ideal for the dynamic production of reports, invoices, tickets, labels, delivery notes, charts, maps etc.
* Vector graphics
* Inclusion of JPEG images
* Object oriented
* Task safe
* Endian-neutral
* Multi-platform, but native code build
* Unconditionally portable code: OS-, CPU-, compiler- independent code
* Pure Ada 2012: this package can be used in projects in Ada 2012 and later language versions

### License

Ada PDF Writer is free, open-source and released under the MIT license.

### Screenshots

![Ada PDF Screenshot](https://apdf.sourceforge.io/pw_ari_delivery_m.png "Screenshot of a page produced by PDF_Out")
![Ada PDF Screenshot](https://apdf.sourceforge.io/pw_sowebio_m.jpg      "Screenshot of a report produced by PDF_Out")
