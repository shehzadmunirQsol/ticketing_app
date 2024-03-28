// pages/pdf.js

import { useState } from 'react';

export default function PdfPage() {
  const [pdfUrl, setPdfUrl] = useState('');

  async function fetchPdf() {
    try {
      const response = await fetch('/api/testing/testing1');
      if (!response.ok) {
        throw new Error('PDF generation failed');
      }
      const pdfBlob = await response.blob();
      const url = URL.createObjectURL(pdfBlob);
      setPdfUrl(url);
    } catch (error) {
      console.error(error);
      // Handle error
    }
  }

  return (
    <div className="w-full relative mx-auto">
      <div className="mx-auto">
        <h1>PDF Viewer</h1>
        <button onClick={fetchPdf}>Generate PDF</button>
        {pdfUrl && (
          <embed
            src={pdfUrl}
            type="application/pdf"
            width="80%"
            height="600px"
          />
        )}
      </div>
    </div>
  );
}
