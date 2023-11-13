import React, { useState } from 'react';
import {
  ColumnDef,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown } from 'lucide-react';
import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import { displayDate } from '~/utils/helper';
import { TableFilters } from './table_filters';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
import { CSVLink } from 'react-csv';
import PlaceholderImage from '~/public/assets/face/male-profile-image-placeholder.png';
import { WinnersEnableDialog, ImageUploadDialog } from '../modal/winnersModal';
import { Switch } from '~/components/ui/switch';
import NextImage from '~/components/ui/img';
export type WinnerType = {
  Event: {
    id: number;
    EventDescription: {
      name: string;
    }[];
    thumb: string;
  };
  Customer: {
    email: string;
    id: number;
    first_name: string;
    phone_number?: string;
  };
  is_cash_alt: boolean;
  thumb?: string;
  is_enabled: boolean;
  draw_date: Date | null;
  ticket_num: string;
  id: number;
};

const initialFilters = {
  first: 0,
  rows: 10,
  lang_id: 1,
  is_admin: true,
};

export default function WinnersDataTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filterID, setFilterID] = useState({});
  const [isEnableModal, setIsEnableModal] = useState(false);
  const [isImageModal, setIsImageModal] = useState(false);
  const [selectedItem, setSelectedItem] = useState<any>({});

  const [filters, setFilters] = useState(initialFilters);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

  const { data, isLoading, refetch } = trpc.winner.get.useQuery(
    { ...filters, filters: { ...filterID } },
    {
      refetchOnWindowFocus: false,
    },
  );

  const winnersData = React.useMemo(() => {
    return Array.isArray(data?.data) && data?.data?.length ? data?.data : [];
  }, [data]);

  const columns: ColumnDef<WinnerType>[] = [
    {
      id: 'ID',
      header: 'Winners ID',
      enableHiding: false,
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="max-w-fit px-2">
                <span className="sr-only">Open menu</span>
                <div className=" hover:text-primary w-14 text-left ">
                  # {row.original.id}
                </div>
                {/* <MoreHorizontal className="h-4 w-4" /> */}
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuItem
                onClick={() => handleEnable(row.original, true)}
              >
                Add Image
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
    {
      id: 'Enabled',
      header: 'Enabled',
      cell: ({ row }) => {
        return (
          <Switch
            checked={row?.original?.is_enabled}
            onCheckedChange={() => handleEnable(row.original, false)}
          />
        );
      },
    },

    {
      accessorKey: 'Product',
      header: 'Product',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <NextImage
              className="object-cover bg-ac-2 h-10 w-16 rounded-lg"
              src={`${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${row?.original?.Event.thumb}`}
              alt={row?.original?.Event.EventDescription[0]?.name ?? ''}
              width={100}
              height={100}
            />

            <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
              {row?.original?.Event.EventDescription[0]?.name}
            </p>
          </div>
        );
      },
    },
    {
      accessorKey: 'Customer Name',
      header: 'Customer Name',
      cell: ({ row }) => (
        <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
          <div className="h-10 w-16 rounded-lg overflow-hidden relative">
            <NextImage
              className="object-cover bg-ac-2 "
              src={
                row?.original?.thumb
                  ? `${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${row?.original?.thumb}`
                  : PlaceholderImage
              }
              alt={row?.original?.Event.EventDescription[0]?.name ?? ''}
              fill
            />
          </div>

          <p className="w-32 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.Customer?.first_name}
          </p>
        </div>
      ),
    },
    {
      accessorKey: 'Draw Date',
      header: 'Draw Date',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap">
          {displayDate(row?.original?.draw_date)}
        </div>
      ),
    },
    {
      accessorKey: 'Customer Email',
      header: 'Customer Email',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap ">
          {row?.original?.Customer?.email}
        </div>
      ),
    },
    {
      accessorKey: 'Phone No',
      header: 'Phone No.',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap ">
          {row?.original?.Customer?.phone_number ?? 'N/A'}
        </div>
      ),
    },

    {
      accessorKey: 'Ticket No',
      header: 'Ticket No.',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap text-primary w-24">
          #{row?.original?.ticket_num}
        </div>
      ),
    },
  ];
  const table = useReactTable({
    data: winnersData as WinnerType[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }
  // FILTER OPTIONS
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'From Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'To Date',
      filtername: 'endDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];

  const csvData = [
    [
      'Winner ID',
      'Product',
      'Customer Name',
      'Customer Email',
      'Phone No.',
      'Draw Date',
      'Ticket No.',
    ],
    ...winnersData?.map(({ id, Event, Customer, draw_date, ticket_num }) => [
      id,
      Event?.EventDescription[0]?.name,
      Customer?.first_name,
      Customer?.email,
      Customer?.phone_number ?? 'N/A',
      draw_date?.toLocaleDateString(),
      '#' + ticket_num,
    ]),
  ];

  function handleEnable(winner: WinnerType, isImage: boolean) {
    setSelectedItem({
      name: winner.Customer.first_name,
      winnerId: winner.id,
      isEnable: winner.is_enabled,
      thumb: winner.thumb,
    });

    if (isImage) {
      setIsImageModal(true);
    } else {
      setIsEnableModal(true);
    }
  }

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-between">
        {winnersData?.length ? (
          <Button variant="outline">
            <CSVLink filename="winners.csv" data={csvData}>
              Export to CSV
            </CSVLink>
          </Button>
        ) : (
          <div />
        )}

        <div className="flex items-center gap-2">
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="outline">
                Columns <ChevronDown className="ml-2 h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              {table
                .getAllColumns()
                .filter((column) => column.getCanHide())
                .map((column) => {
                  return (
                    <DropdownMenuCheckboxItem
                      key={column.id}
                      className="capitalize"
                      checked={column.getIsVisible()}
                      onCheckedChange={(value) =>
                        column.toggleVisibility(!!value)
                      }
                    >
                      {column.id}
                    </DropdownMenuCheckboxItem>
                  );
                })}
            </DropdownMenuContent>
          </DropdownMenu>
          <TableFilters
            inputList={roleOptions1}
            item_name={'Winners'}
            value={filterID}
            setValue={setFilterID}
            setFilters={setFilters}
            initial={initialFilters}
          />
        </div>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader className="bg-secondary/80">
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((data?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
              </SelectTrigger>
              <SelectContent side="top">
                {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                  <SelectItem key={pageSize} value={`${pageSize}`}>
                    {pageSize}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to next page</span>
              <ChevronRightIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() =>
                handlePagination(
                  Math.ceil((data?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>

      <LoadingDialog open={isLoading} text={'Loading data...'} />
      <WinnersEnableDialog
        {...{
          selectedItem,
          setSelectedItem,
          isEnableModal,
          setIsEnableModal,
          refetch,
        }}
      />
      <ImageUploadDialog
        {...{
          selectedItem,
          setSelectedItem,
          isImageModal,
          setIsImageModal,
          refetch,
        }}
      />
    </div>
  );
}
